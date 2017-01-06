module Main where

import Vector
import Graphics.UI.GLUT
import Snowflake


vertex2f :: Vector -> IO()
vertex2f (Vector x y) = vertex $ Vertex3 0.0 ((realToFrac x) :: GLfloat) ((realToFrac y) :: GLfloat)
go :: IO()
go = do
	(_progName, _args) <- getArgsAndInitialize
	initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
	_window <- createWindow "Hello World"
	color $ Color3 (100 :: GLfloat) 100 100
	displayCallback $= display
	mainLoop
	
display :: DisplayCallback
display = do
	clear [ColorBuffer]
	let lines = initialLines in 
		let newLines = (lines ++ (iterateSnowflake lines)) in do
			putStrLn (show $ apply lineToV newLines)
			renderPrimitive Lines (mapM_ vertex2f (apply lineToV newLines))
			flush
