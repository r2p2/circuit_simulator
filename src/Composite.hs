module Composite (CompositeCircuit, makeNand) where

import Data.List (intercalate)
import IntegratedCircuit
import LogicGates

data CompositeCircuit = CompositeCircuit CircuitState [String] [String]

instance Show CompositeCircuit where
    show (CompositeCircuit _ inputs outputs)
        = "CompositeCircuit(" ++ (intercalate ", " inputs) ++
          " -> " ++ (intercalate ", " outputs) ++ ")"


instance IntegratedCircuitComponent CompositeCircuit where
    tick (CompositeCircuit st inputs outputs) inputPins
        = let (outputPins, st') = runState st $
                                  do mapM_ (uncurry setInput) $ zip inputs inputPins
                                     tickState
                                     mapM getOutput outputs
          in (CompositeCircuit st' inputs outputs, outputPins)


makeNand :: CompositeCircuit
makeNand = let st = new $
                    do addComponent AndGate ["in1", "in2"] ["out1"]
                       addComponent NotGate ["in3"] ["out2"]
                       connect "out1" "in3"
           in CompositeCircuit st ["in1", "in2"] ["out2"]
