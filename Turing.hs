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
  start :: State
  }
