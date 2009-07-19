module Main where

import Control.Monad (forM)
import IntegratedCircuit
import LogicGates
import Clock
import Composite

main :: IO ()
main = do let initialState = build
          loop initialState

loop :: CircuitState -> IO ()
loop st = do let ((t, outs), st') =
                     runState st $
                              do tickState
                                 outs' <- forM [0..8] $ getOutput . ("counter-output" ++) . show
                                 t' <- getTime
                                 return (t', outs')
             putStrLn $ (show t) ++ ": " ++ (show outs)
             loop st'


build :: CircuitState
build = new $
        do addComponent (ConstGate True) [] ["const-out"]
           addComponent (makeCounter width) ["counter-trigger"] (names "counter-output")
           connect "const-out" "counter-trigger"
    where width = 8
          w = width - 1
          range = [0..w]
          names prefix = map ((prefix ++) . show) range
