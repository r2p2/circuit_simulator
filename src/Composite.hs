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
    where w = width - 1
          range = [0..w]
          names prefix = map ((prefix ++) . show) range

-- http://en.wikipedia.org/wiki/Adder_%28electronics%29#Full_adder
makeFullAdder :: CompositeCircuit
makeFullAdder = makeComposite ["a", "b", "c-in"] ["s", "c-out"] $
                do addComponent XorGate ["a", "b"] ["xor1-out"]
                   addComponent XorGate ["xor2-in1", "c-in"] ["s"]
                   addComponent AndGate ["a", "b"] ["and1-out"]
                   addComponent AndGate ["and2-in1", "c-in"] ["and2-out"]
                   addComponent OrGate ["or-in1", "or-in2"] ["c-out"]
                   connect "xor1-out" "xor2-in1"
                   connect "xor1-out" "and2-in1"
                   connect "and1-out" "or-in1"
                   connect "and2-out" "or-in2"

-- http://en.wikipedia.org/wiki/Adder_%28electronics%29#Ripple_carry_adder
makeRippleCarryAdder :: Int  -- ^bit width
                     -> CompositeCircuit
makeRippleCarryAdder width = makeComposite ((names "a") ++ (names "b"))
                                           (names "s") $
                             do foldM_ (\prevC i ->
                                            do let iS = show i
                                               addComponent makeFullAdder
                                                            ["a" ++ iS, "b" ++ iS, "ci" ++ iS]
                                                            ["s" ++ iS, "co" ++ iS]
                                               connect prevC ("ci" ++ iS)
                                               return $ "co" ++ iS
                                       ) "cNull" range
    where w = width - 1
          range = [0..w]
          names prefix = map ((prefix ++) . show) range

makeCounter :: Int
            -> CompositeCircuit
makeCounter width = makeComposite ["trigger"] (names "o") $
                    do addComponent (makeRegister width)
                                    ("trigger":(names "reg-in"))
                                    (names "reg-out")
                       addComponent (makeRippleCarryAdder width)
                                    ((replicate width "trigger") ++ (names "add-b"))
                                    (names "o")
                       forM_ range $ \i ->
                           do let iS = show i
                              connect ("add-s" ++ iS) ("reg-in" ++ iS)
                              connect ("reg-out" ++ iS) ("add-b" ++ iS)
    where w = width - 1
          range = [0..w]
          names prefix = map ((prefix ++) . show) range
