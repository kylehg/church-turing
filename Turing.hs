-- Code for representing Turing machines in Haskell
-- Author: Kyle Hardgrave (kyleh@seas)

module Turing where

import qualified Data.Set as Set


type TMState = Int
type Alphabet = Char
data Dir = L | R deriving (Eq, Show)

-- | A Turing machine
data TM = TM {
  -- The possible states
  states :: Set.Set TMState,
  -- The transition function
  trans  :: (TMState, Alphabet) -> (TMState, Alphabet, Dir),
  -- The beginning state
  start  :: TMState,
  end    :: TMState
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
move :: Dir -> Tape -> Tape
move R (Tape ls (c:rs)) = Tape (c:ls) rs
move R (Tape ls [])     = Tape (blank:ls) []
move L (Tape (c:ls) rs) = Tape ls (c:rs)
move L t                = t

-- | Write a symbol at the head position of the tape and move
write :: Alphabet -> Dir -> Tape -> Tape
write c d (Tape ls (_:rs)) = move d $ Tape ls (c:rs)
write c d (Tape ls [])     = move d $ Tape ls [c]

-- | Run a TM until it hits an end state and return the resulting tape.
runTM :: TM -> Tape -> Tape
runTM m t = t' where (_, t') = runFromState m (start m, t)

-- | Given a TM and its state and tape position, run it until it reaches an
-- end state.
runFromState :: TM -> (TMState, Tape) -> (TMState, Tape)
runFromState m (s, t) | s == end m = (s, t)
                      | otherwise  = runFromState m (s', t')
  where t' = write c d t
        (s', c, d) = trans m (s, getC t)
