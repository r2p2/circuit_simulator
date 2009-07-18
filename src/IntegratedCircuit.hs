
module IntegratedCircuit (Pin, Register, IntegratedCircuitComponent, tick) where

import Data.Word

type Pin = Bool
type Register = Word16

class IntegratedCircuitComponent a where
	tick :: a -> a