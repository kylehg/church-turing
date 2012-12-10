{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE UndecidableInstances #-}

import           Network.CGI
import           Text.XHtml

import           Control.Applicative
import           Control.Monad (mzero, join)
import           Control.Monad.Trans.Maybe
import           Control.Monad.Reader
import           Data.List
import qualified Data.Map as M
import           Data.Maybe (fromMaybe)
import           Data.Monoid
import           Safe (readDef)
import           Test.QuickCheck hiding (output)
import           Text.Parsec hiding (getInput, (<|>), many, space, spaces)
import           Text.Parsec.String (Parser)
import qualified Unbound.LocallyNameless as LN
import           Unbound.LocallyNameless hiding (name, join)

------------------------------------------------------------
-- LC types
------------------------------------------------------------

data Term = Var (Name Term)
          | Lam (Bind (Name Term) Term)
          | App Term Term
  deriving (Show)
$(derive [''Term])
instance Alpha Term
instance Subst Term Term where
  isvar (Var v) = Just (SubstName v)
  isvar _       = Nothing

var :: String -> Term
var x = Var (s2n x)

f = var "f"
x = var "x"
y = var "y"
z = var "z"

lam :: String -> Term -> Term
lam x b = Lam $ bind (s2n x) b

app :: Term -> Term -> Term
app = App

type Defn = (String, Term)
type Defns = M.Map String Term
data Prog = Prog Defns Term
  deriving Show

addDefn :: Defn -> Prog -> Parser Prog
addDefn (x,def) (Prog m t)
  | x `M.member` m
    = parserFail $ "Duplicate definition of " ++ x
  | x `elem` (map anyName2String $ fvAny def)
    = parserFail $ "Illegal recursive definition of " ++ x
  | otherwise = return $ Prog (M.insert x def m) t

------------------------------------------------------------
-- Parsing
------------------------------------------------------------

space   = char ' ' <|> char '\t'
spaces  = many space
spaces1 = many1 space

parseId :: Parser String
parseId = spaces *> ((:) <$> letter <*> many alphaNum) <* spaces

parseVar, parseLC, parseLCLam, parseLCAtom :: Parser Term

parseVar = var <$> parseId

parseLC = spaces *> (parseLCLam <|> chainl1 parseLCAtom (spaces *> pure app)) <* spaces

parseLCLam = lam <$> ((char '\\' <|> char 'λ') *> spaces *> parseId)
                 <*> (spaces *> char '.' *> spaces *> parseLC)

parseLCAtom = parseVar <|> parens parseLC

parens = between (char '(') (char ')')

parseAlg :: Parser Term
parseAlg = spaces *>
           ( var <$> (string "Var" *> spaces *> parens (parseId))
         <|> lam <$> (string "Lam" *> spaces *> char '(' *> parseId) <*> (char ',' *> spaces *> parseAlg <* spaces <* char ')')
         <|> app <$> (string "App" *> spaces *> char '(' *> parseAlg) <*> (spaces *> char ',' *> spaces *> parseAlg <* spaces <* char ')')
           )

parseLCAny = try parseLC <|> parseAlg
parseLCLine = parseLCAny <* spaces <* ((pure () <* eol) <|> eof)
eol = (char '\n' >> return ()) <|> (string "\r\n" >> return ())

parseDefn :: Parser (String, Term)
parseDefn = (,) <$> (spaces *> parseId) <*> (spaces *> char '=' *> spaces *> parseLCLine)

parseProg :: Parser Prog
parseProg = Prog M.empty <$> try parseLCLine
        <|> join (addDefn <$> parseDefn <*> parseProg)

------------------------------------------------------------
-- Pretty-printing
------------------------------------------------------------

ppTermLC :: Term -> String
ppTermLC = runLFreshM . ppTermLC' 0
  where
    ppTermLC' :: Int -> Term -> LFreshM String
    ppTermLC' _ (Var x) = return (show x)
    ppTermLC' p (Lam l) = do
      lunbind l $ \(x,t) -> do
      t' <- ppTermLC' 0 t
      return $ showParens (p > 0) ("λ" ++ show x ++ ". " ++ t')
    ppTermLC' p (App t1 t2) = do
      t1' <- ppTermLC' 1 t1
      t2' <- ppTermLC' 2 t2
      return $ showParens (p > 1) (t1' ++ " " ++ t2')

showParens True x = "(" ++ x ++ ")"
showParens _    x = x

ppTermAlg :: Term -> String
ppTermAlg = runLFreshM . ppTermAlg'
  where
    ppTermAlg' :: Term -> LFreshM String
    ppTermAlg' (Var x) = return $ cons "Var" [show x]
    ppTermAlg' (Lam l) = do
      lunbind l $ \(x,t) -> do
      t' <- ppTermAlg' t
      return $ cons "Lam" [show x, t']
    ppTermAlg' (App t1 t2) = cons "App" <$> (mapM ppTermAlg' [t1,t2])

cons k args = k ++ "(" ++ intercalate ", " args ++ ")"

------------------------------------------------------------
-- Evaluation
------------------------------------------------------------

step :: Term -> MaybeT (ReaderT Defns LFreshM) Term
step (Var _)         = mzero
step (Lam _)         = mzero
step (App (Var x) t) = do
  mx' <- asks (M.lookup (name2String x))
  case mx' of
    Nothing -> App <$> pure (Var x) <*> step t
    Just x' -> return (App x' t)
step (App (Lam l) t) = do
  lunbind l $ \(x,b) -> do
  return $ subst x t b
step (App t1 t2)     = App <$> step t1 <*> pure t2
                   <|> App <$> pure t1 <*> step t2

steps :: Int -> Term -> ReaderT Defns LFreshM [Term]
steps 0 t = return [t]
steps n t = do
  mt' <- runMaybeT (step t)
  case mt' of
    Nothing -> return [t]
    Just t' -> (t:) <$> steps (n-1) t'

------------------------------------------------------------
-- QC tests
------------------------------------------------------------

instance Arbitrary Term where
  arbitrary = sized term
    where
      v      = (:[]) <$> elements "vwxyz"
      term 0 = var <$> v
      term n = oneof [ lam <$> v <*> term (n-1)
                     , app <$> term (n `div` 2) <*> term (n `div` 2)
                     ]

prop_LC_pp_parse t =
  case parse parseLC "" (ppTermLC t) of
    Left _ -> False
    Right t' -> aeq t t'

prop_Alg_pp_parse t =
  case parse parseAlg "" (ppTermAlg t) of
    Left _ -> False
    Right t' -> aeq t t'

------------------------------------------------------------
-- CGI
------------------------------------------------------------

inputForm inp numSteps = form
            << [ paragraph
                 << (instructions +++ br +++ textarea (toHtml inp)
                                          ! [name "input", rows "5", cols "80"])
               , submit "lc2alg" "LC -> Algebra"
               , submit "alg2lc" "Algebra -> LC"
               , br
               , submit "run" "Run"
               , toHtml " for at most "
               , textfield "numSteps" ! [value numSteps]
               , toHtml " steps"
               ]

instructions = unlines
  [ "Enter a lambda calculus term below using abbreviated notation and click"
  , "\"LC -> Algebra\" to convert it to a term in the full algebra; or enter"
  , "a term in the algebra to click \"Algebra -> LC\" to convert it to abbreviated"
  , "form. You can also evaluate a lambda calculus term.  You may optionally enter"
  , "definitions in the form \"<name> = <term>\", one definition per line. The"
  , "last line must be a lambda calculus term, whose evaluation will be shown by"
  , "steps when you click \"Run\"."
  ]

page t b = header << thetitle << t +++ body << b

cgiMain = do inp      <- getInput "input"
             lc2alg   <- getInput "lc2alg"
             alg2lc   <- getInput "alg2lc"
             run      <- getInput "run"
             numSteps <- getInput "numSteps"
             let out = case (lc2alg, alg2lc, run) of
                         (Just _, _, _) -> showLC2Alg inp
                         (_, Just _, _) -> showAlg2LC inp
                         (_, _, Just _) -> showEval numSteps inp
                         _              -> noHtml
             output
               . renderHtml
               $ page "The lambda calculator"
                   (inputForm (fromMaybe "" inp) (fromMaybe "" numSteps)
                      +++ hr +++ "Output: " +++ br +++ thecode << out)

showLC2Alg Nothing = noHtml
showLC2Alg (Just inp) =
  case parse parseLC "" inp of
    Left _  -> toHtml "Parse error"
    Right t -> toHtml (ppTermAlg t)

showAlg2LC Nothing = noHtml
showAlg2LC (Just inp) =
  case parse parseAlg "" inp of
    Left _  -> toHtml "Parse error"
    Right t -> toHtml (ppTermLC t)

showEval _ Nothing = noHtml
showEval numSteps (Just inp) =
  case parse parseProg "" inp of
    Left pe -> htmlLines . lines . show $ pe
    Right p -> runProg nsteps p
 where
  nsteps = readDef 100 (fromMaybe "100" numSteps)

runProg nsteps (Prog defs t)
  = htmlLines
  . map ppTermLC
  . runLFreshM
  . flip runReaderT defs
  . steps nsteps
  $ t

htmlLines = mconcat . intersperse br . map toHtml

main = runCGI $ handleErrors cgiMain
