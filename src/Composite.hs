module Composite (CompositeCircuit, makeNand,
                  makeRSFlipFlop, makeDFlipFlop, makeRegister,
                  makeFullAdder, makeRippleCarryAdder,
                  makeCounter) where

import Data.List (intercalate)
import Control.Monad (forM_, foldM_)
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


makeComposite :: [String]  -- ^inputs
              -> [String]  -- ^outputs
              -> Circuit a
              -> CompositeCircuit
makeComposite inputs outputs builder
    = let st = new builder
      in CompositeCircuit st inputs outputs

makeNand :: CompositeCircuit
makeNand = makeComposite ["in1", "in2"] ["out2"] $
           do addComponent AndGate ["in1", "in2"] ["out1"]
              addComponent NotGate ["in3"] ["out2"]
              connect "out1" "in3"

makeRSFlipFlop :: CompositeCircuit
makeRSFlipFlop = makeComposite ["ns", "nr"] ["q", "nq"] $
                 do addComponent makeNand ["ns", "back1"] ["q"]
                    addComponent makeNand ["back2", "nr"] ["nq"]
                    connect "q" "back2"
                    connect "nq" "back1"

{- http://hyperphysics.phy-astr.gsu.edu/Hbase/Electronic/dflipflop.html
                  ,----.   .____.
  [D]--.----------|NAND|---|NAND|----[Q]
       |         ,|____|  /`----')
       | [Clock]-| ____   ===x===
       |  ___    `|    |  \.----.)
       `-|NOT|----|NAND|---|NAND|----[NQ]
         `---'    `----'   `----'
-}
makeDFlipFlop :: CompositeCircuit
makeDFlipFlop = makeComposite ["clock", "data"] ["q", "nq"] $
                do addComponent NotGate ["data"] ["not-out"]
                   addComponent makeNand ["data", "clock"] ["nand1-out"]
                   addComponent makeNand ["clock", "nand2-in2"] ["nand2-out"]
                   addComponent makeRSFlipFlop ["rs-ns", "rs-nr"] ["q", "nq"]
                   connect "not-out" "nand2-in2"
                   connect "nand1-out" "rs-ns"
                   connect "nand2-out" "rs-nr"

-- http://de.wikipedia.org/wiki/Flipflop#Breite_Verwendung_in_der_Digitalelektronik
makeRegister :: Int  -- ^bit width
             -> CompositeCircuit
makeRegister width = makeComposite ("clock":(names "d"))
                                   (names "q") $
                     do forM_ range $ \i ->
                            do let iS = show i
                                   clock = "clock" ++ iS
                                   d = "d" ++ iS
                                   q = "q" ++ iS
                                   nq = "nq" ++ iS
                               addComponent makeDFlipFlop [clock, d] [q, nq]
                               connect "interface-clock" clock
                               connect ("interface-d" ++ iS) d
                               connect q ("interface-q" ++ iS)
    where w = width - 1
          range = [0..w]
          names prefix = map ((prefix ++) . show) range
