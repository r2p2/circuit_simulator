
module Clock where

import IntegratedCircuit

type Every = Int
type Counter = Int

data Clock = Clock Every Counter Pin deriving (Show)     -- o1

instance IntegratedCircuitComponent Clock where
--  tick (Clock every every o1) = Clock every 0 (not o1)
--  tick (Clock every counter o1) = Clock every (counter+1) o1
  tick (Clock every counter o1)
    | every == counter = Clock every 0 (not o1)
    | otherwise        = Clock every (counter+1) o1