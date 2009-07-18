
module LogicGates where

import IntegratedCircuit

data NotGate = NotGate Pin Pin deriving (Show)     -- i1 o1    -- o1 = not i1 
data AndGate = AndGate Pin Pin Pin deriving (Show) -- i1 i2 o1 -- o1 = i1 AND i2
data OrGate  = OrGate Pin Pin Pin deriving (Show)  -- i1 i2 o1 -- o1 = i1 AND i2
data XorGate = XorGate Pin Pin Pin deriving (Show) -- i1 i2 o1 -- o1 = i1 XOR i2

instance IntegratedCircuitComponent NotGate where
	tick (NotGate i1 o1) = NotGate i1  (not i1)

instance IntegratedCircuitComponent AndGate where
	tick (AndGate i1 i2 o1) = AndGate i1 i2 (i1 && i2)

instance IntegratedCircuitComponent OrGate where
	tick (OrGate i1 i2 o1) = OrGate i1 i2 (i1 || i2)
  
instance IntegratedCircuitComponent XorGate where
	tick (XorGate i1 i2 o1) =
		XorGate i1 i2 o1
		where
			(OrGate _ _ o1) = tick (OrGate ao1 ao2 False)
			(AndGate _ _ ao2) = tick (AndGate i1 no2 False)
			(AndGate _ _ ao1) = tick (AndGate no1 i2 False)
			(NotGate _ no2) = tick (NotGate i2 False)
			(NotGate _ no1) = tick (NotGate i1 False)
  