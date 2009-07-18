
module Main where

import IntegratedCircuit
import LogicGates
import Clock

main = do
  run (Clock 50 0 False) (Clock 100 0 False) (Clock 25 0 False) (Clock 75 0 False)

{-             ___
 [ 50 ] in1 --|   |out1
              | & |-----.   ___
 [100 ] in2 --|___|     `--|>=1|out0
               ___         |   |---
 [ 25 ] in3 --|   |out2 .--|___|
              | & |-----´
 [ 75 ] in4 --|___|
-}

run :: Clock -> Clock -> Clock -> Clock -> IO ()
run clock1 clock2 clock3 clock4 = do
  print ("in1: " ++ (show in1))
  print ("in2: " ++ (show in2))
  print ("in3: " ++ (show in3))
  print ("in4: " ++ (show in4))
  print out0
  print ""
  run (tick clock1) (tick clock2) (tick clock3) (tick clock4)
  where
    (OrGate _ _ out0)  = tick (OrGate out1 out2 True)
    (AndGate _ _ out1) = tick (AndGate in1 in2 True)
    (AndGate _ _ out2) = tick (AndGate in3 in4 True)
    
    (Clock _ _ in1) = clock1
    (Clock _ _ in2) = clock2
    
    (Clock _ _ in3) = clock3
    (Clock _ _ in4) = clock4
  