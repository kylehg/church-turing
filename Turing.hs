-- Code for representing Turing machines in Haskell
-- Author: Kyle Hardgrave (kyleh@seas)

module Turing where

import qualified Data.Set as Set


type TMState = Int
type Alphabet = Char
type Move = [Action]
data Action = R          -- Read right
            | L          -- Read left
            | E          -- Erase and shift the following symbols back
            | P Alphabet -- Print over the current symbol
            | I Alphabet -- Print and shift the following symbols over
            deriving (Eq, Show)

-- | A Turing machine
data TM = TM {
  -- The possible states
  states :: Set.Set TMState,
  -- The transition function
  trans  :: (TMState, Alphabet) -> (TMState, Move),
  -- The beginning state
  start  :: TMState,
  end    :: TMState --Maybe TMState
  }

-- | The tape to be read by a turing machine
data Tape = Tape [Alphabet] [Alphabet] deriving (Eq, Show)


-- | The blank symbol
blank :: Alphabet
blank = '_'

-- | Convert a list of symbols to a Tape
tapeFromList :: [Alphabet] -> Tape
tapeFromList cs = Tape [] cs

-- | Convert a Tape back into a list of symbols
listFromTape :: Tape -> [Alphabet]
listFromTape (Tape ls rs) = ls ++ rs

-- | Read a symbol from the tape
getC :: Tape -> Alphabet
getC (Tape _ (c:cs)) = c
getC (Tape _ [])     = '_'

-- | Move the TM head along the Tape in a particular direction
move :: Action -> Tape -> Tape
move R (Tape ls (c:rs))   = Tape (c:ls) rs
move R (Tape ls [])       = Tape (blank:ls) [] -- Read blanks at end
move L (Tape (c:ls) rs)   = Tape ls (c:rs)
move L t                  = t                  -- Stop at beginning
move E (Tape ls (c:rs))   = Tape ls rs
move E t                  = t                  -- Nothing to erase @ end
move (P x) (Tape ls (c:rs)) = Tape ls (x:rs)
move (P x) (Tape ls [])     = Tape ls [x]
move (I x) (Tape ls rs)     = Tape ls (x:rs)

-- | Run a TM until it hits an end state and return the resulting tape.
runTM :: TM -> Tape -> Tape
runTM m t = t' where (_, t') = runFromState m (start m, t)

-- | Given a TM and its state and tape position, run it until it reaches an
-- end state.
runFromState :: TM -> (TMState, Tape) -> (TMState, Tape)
runFromState m (s, t) | done      = (s, t)
                      | otherwise = runFromState m (s', t')
  where done = s == end m --maybe False (==s) (end m)
        t' = foldl (\t0 a -> move a t0) t as
        (s', as) = trans m (s, getC t)
