module Main where

import IntegratedCircuit
import LogicGates
import Clock

main :: IO ()
main = do let initialState = build
          loop initialState

loop :: CircuitState -> IO ()
loop st = do let ((t, out0), st') =
                     runState st $
                              do tickState
                                 out0' <- getOutput "out0"
                                 t' <- getTime
                                 return (t', out0')
             putStrLn $ (show t) ++ ": out0=" ++ (show out0)
             loop st'

{-             ___
 [ 50 ] in1 --|   |out1
              | & |-----.   ___
 [100 ] in2 --|___|     `--|   |out0
                        in5|>=1|---
               ___      in6|   |
 [ 25 ] in3 --|   |out2 .--|___|
              | & |-----´
 [ 75 ] in4 --|___|
-}

build :: CircuitState
build = snd $
        runState newState $
        do addComponent (makeClock 50) [] ["t1"]
           addComponent (makeClock 100) [] ["t2"]
           addComponent (makeClock 25) [] ["t3"]
           addComponent (makeClock 75) [] ["t4"]
           addComponent AndGate ["in1", "in2"] ["out1"]
           addComponent AndGate ["in3", "in4"] ["out2"]
           addComponent OrGate ["in5", "in6"] ["out0"]
           connect "t1" "in1"
           connect "t2" "in2"
           connect "t3" "in3"
           connect "t4" "in4"
           connect "out1" "in5"
           connect "out2" "in6"
