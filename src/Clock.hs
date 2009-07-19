module Clock (makeClock, Clock) where

import IntegratedCircuit

type Every = Int
type Counter = Int

data Clock = Clock Every Counter Pin
             deriving (Show)

makeClock :: Every -> Clock
makeClock every = Clock every 0 False

instance IntegratedCircuitComponent Clock where
  tick (Clock every counter pin) []
    | every <= counter = let pin' = not pin
                         in (Clock every 0 pin', [pin'])
    | otherwise        = let counter' = counter + 1
                         in (Clock every counter' pin, [pin])
