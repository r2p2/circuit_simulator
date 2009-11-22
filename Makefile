all:
	ghc -package mtl -package containers  -outputdir bin  src/IntegratedCircuit.hs src/LogicGates.hs src/Composite.hs src/Clock.hs src/Main.hs
