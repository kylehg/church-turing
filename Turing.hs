-- Code for representing Turing machines in Haskell
-- Author: Kyle Hardgrave (kyleh@seas)

{-# LANGUAGE GADTS #-}

module Turing where

import qualified Data.Set as Set



type State = Int
type Alphabet = Char
data Dir = L | R deriving Eq, Show

-- | A Turing machine
data TM = TM {
  -- The possible states
  states :: Set State,
  -- The transition function
  trans :: (State, Alphabet) -> (State, Alphabet, Dir),
  -- The beginning state
  start :: State,
  accept :: Set State,
  reject :: Set State
  }

-- | The tape to be read by a turing machine
data Tape = Tape [Alphabet] [Alphabet]

-- | The blank symbol
blank :: Alphabet
blank = '_'

-- | Convert a list of symbols to a Tap
tapeFromList :: [Alphabet] -> Tape
tapeFromList cs = Tape [] cs

-- | Read a symbol from the tape
getC :: Tape -> Alphabet
getC Tape _ (c:cs) = c
getC Tape _ []     = '_'

-- | Move the TM head along the Tape in a particular direction
move :: Dir -> Tape -> Tape
move R (Tape ls (c:rs)) = Tape (c:ls) rs
move R (Tape ls [])     = Tape (blank:ls) rs
move L (Tape (c:ls) rs) = Tape ls (c:rs)
move L t                = t

-- | Write a symbol at the head position of the tape and move
write :: Alphabet -> Dir -> Tape -> Tape
write c d (Tape ls (_:rs)) = move d $ Tape ls (c:rs)
write c d (Tape ls [])     = move d $ Tape ls [c]

-- | Run a TM
runTM :: TM -> Tape -> (State, Tape)
runTM = undefined