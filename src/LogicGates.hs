module LogicGates (LogicGate(..)) where

import IntegratedCircuit

data LogicGate = NotGate
               | AndGate
               | OrGate
               | XorGate
                 deriving (Show)

instance IntegratedCircuitComponent LogicGate where
	tick NotGate [i1] = (NotGate, [not i1])
	tick AndGate [i1, i2] = (AndGate, [i1 && i2])
	tick OrGate [i1, i2] = (OrGate, [i1 || i2])
	tick XorGate [i1, i2] = (XorGate, [o1])
	    where o1 = (i1 || i2) && not (i1 && i2)
  