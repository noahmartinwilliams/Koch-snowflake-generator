module Main where

import Vector
import Graphics.UI.GLUT
import Snowflake


vertex2f :: Vector -> IO()
vertex2f (Vector x y) = vertex $ Vertex3 ((realToFrac x) :: GLfloat) ((realToFrac y) :: GLfloat) 0.0
go :: IO()
go = do
	(_progName, _args) <- getArgsAndInitialize
	_window <- createWindow "Hello World"
	displayCallback $= display
	mainLoop

main :: IO()
main = go
	
display :: DisplayCallback
display = do
	clear [ColorBuffer]
	let lines = initialLines in 
		let newLines = (foldr (++) [] (apply lineToVs (iterateTil 0.01 initialLines))) in do
			putStrLn $ show (length newLines)
			putStrLn $ show newLines
			renderPrimitive Lines $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) (apply (\(Vector x y) -> (((realToFrac x) :: GLfloat), ((realToFrac y) :: GLfloat), 0.0)) newLines)
			flush
