module Main where

import Vector
import Graphics.UI.GLUT
import Snowflake
import Data.IORef


vertex2f :: Vector -> IO()
vertex2f (Vector x y) = vertex $ Vertex3 ((realToFrac x) :: GLfloat) ((realToFrac y) :: GLfloat) 0.0

go :: IO()
go = do
	(_progName, _args) <- getArgsAndInitialize
	initialDisplayMode $= [WithDepthBuffer, DoubleBuffered]
	_window <- createWindow "Hello World"
	reshapeCallback $= Just reshape
	depthFunc $= Just Less
	pos <- newIORef (0.0, 0.0, 0.0)
	size <- newIORef 1.0
	depth <- newIORef 2
	keyboardMouseCallback $= Just (keyboardMouse pos size depth)
	displayCallback $= display pos size depth
	idleCallback $= Just idle
	mainLoop


idle :: IdleCallback
idle = do
	postRedisplay Nothing

keyboardMouse :: IORef (GLfloat, GLfloat, GLfloat) -> IORef GLfloat -> IORef Int -> KeyboardMouseCallback
keyboardMouse p s d key _ _ _ = do
	pos <- get p
	scale <- get s
	depth <- get d
	let (x, y, z) = pos in
		case key of
			(SpecialKey KeyLeft) -> p $= (x + 0.1, y, z)
			(SpecialKey KeyRight) -> p $= (x - 0.1, y, z)
			(SpecialKey KeyUp) -> p $= (x, y - 0.1 , z)
			(SpecialKey KeyDown) -> p $= (x, y + 0.1 , z)
			(MouseButton WheelUp) -> do s $= scale + 0.1 ; d $= (round ((scale + 0.1) / 1.5))
			(MouseButton WheelDown) -> do s $= scale - 0.1 ; d $= (round ((scale - 0.1)/ 1.5))
			_ -> return()

main :: IO()
main = go

reshape :: ReshapeCallback
reshape size = do
	viewport $= (Position 0 0, size)
	
display :: IORef (GLfloat, GLfloat, GLfloat) -> IORef GLfloat -> IORef Int -> DisplayCallback
display pos s d = do
	depth <- get d
	let lines = initialLines in 
		let newLines = (foldr (++) [] (map lineToVs (iterateDepth depth initialLines))) in do
			clear [ColorBuffer, DepthBuffer]
			clear [ColorBuffer]
			loadIdentity
			(x', y', z') <- get pos
			size <- get s
			translate $ Vector3 x' y' z'
			preservingMatrix $ do 
				renderPrimitive Lines $ mapM_ (\(x, y, z) -> vertex $ Vertex3 x y z) (map (\(Vector x y) -> ((((realToFrac x) :: GLfloat) * size), (((realToFrac y) :: GLfloat) * size), 0.0)) newLines)
			swapBuffers
