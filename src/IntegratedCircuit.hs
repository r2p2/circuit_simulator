{-# LANGUAGE ExistentialQuantification,ImpredicativeTypes, RankNTypes #-}
module IntegratedCircuit (Pin, Register, IntegratedCircuitComponent, tick,
                          newState, addComponent, connect,
                          CircuitState, Circuit,
                          runState, tickState, setInput, getOutput, getTime
                         ) where

import Data.Word
import Control.Monad.State.Lazy hiding (runState)
import qualified Control.Monad.State.Lazy as St (runState)
import qualified Data.Map as Map


type Pin = Bool
type Register = Word16

class Show a => IntegratedCircuitComponent a where
	tick :: a -> [Pin] -> (a, [Pin])


data StateComponent = forall c. (IntegratedCircuitComponent c, Show c)
    => Component c [String] [String]

data CircuitState = CircuitState { stateComponents :: [StateComponent],
                                   stateConnections :: [(String, String)],
                                   stateInputs :: Map.Map String Pin,
                                   stateOutputs :: Map.Map String Pin,
                                   stateTime :: Integer
                                 }

type Circuit a = State CircuitState a

newState :: CircuitState
newState = CircuitState [] [] Map.empty Map.empty 0

modifyState :: (CircuitState -> CircuitState) -> Circuit ()
modifyState f = withState f $ return ()

runState :: CircuitState -> Circuit a -> (a, CircuitState)
runState = flip St.runState

addComponent :: IntegratedCircuitComponent c => c  -- ^component
             -> [String]  -- ^input names
             -> [String]  -- ^output names
             -> Circuit ()
addComponent component inputs outputs
    = modifyState $
      \st -> st { stateComponents =
                      (Component component inputs outputs)
                      :(stateComponents st)
                }
      
         

connect :: String  -- ^output name
        -> String  -- ^input name
        -> Circuit ()
connect input output
    = modifyState $
      \st -> st { stateConnections = (input, output):(stateConnections st) }

tickState :: Circuit ()
tickState
    = -- Tick components, inputs to outputs
      do components <- stateComponents `liftM` get
         newComponents <- forM components $
                          \(Component component inputs outputs) ->
                              do inputPins <- mapM getInput inputs
                                 let (component', outputPins) = tick component inputPins
                                 forM_ (zip outputs outputPins) (uncurry setOutput)
                                 return $ Component component' inputs outputs
         -- Don't reuse previously gotten state
         -- because we already modified it via setOutput
         st <- get
         put st { stateComponents = newComponents }

         -- Transmit connections
         let connections = stateConnections st
         forM_ connections $
                   \(output, input) ->
                       getOutput output >>=
                       setInput input
         -- Done
         modifyState $ \st' -> st' { stateTime = stateTime st' + 1 }
         return ()

getInput :: String -> Circuit Pin
getInput input
    = (maybe False id .
       Map.lookup input .
       stateInputs) `liftM`
      get

setInput :: String -> Pin -> Circuit ()
setInput input pin
    = modifyState $
      \st -> st { stateInputs = Map.insert input pin $ stateInputs st }

getOutput :: String -> Circuit Pin
getOutput output
    = (maybe False id .
       Map.lookup output .
       stateOutputs) `liftM`
      get

setOutput :: String -> Pin -> Circuit ()
setOutput output pin
    = modifyState $
      \st -> st { stateOutputs = Map.insert output pin $ stateOutputs st }

getTime :: Circuit Integer
getTime = stateTime `liftM` get
