module Vector where

data Vector = Vector Double Double 
	deriving (Ord, Show, Eq)

data Line = Line Vector Vector 
	deriving (Ord, Show, Eq)

addv :: Vector -> Vector -> Vector
subv :: Vector -> Vector -> Vector
mulv :: Vector -> Double -> Vector
divv :: Vector -> Double -> Vector
absv :: Vector -> Double
invv :: Vector -> Vector

lineToV :: Line -> Vector
lineToVs :: Line -> [Vector]
absl :: Line -> Double
divl :: Line -> Double -> Line


addv (Vector x1 y1) (Vector x2 y2) = Vector (x1 + x2) (y1 + y2)
subv (Vector x1 y1) (Vector x2 y2) = Vector (x1 - x2) (y1 - y2)
mulv (Vector x1 y1) s = Vector (x1 * s) (y1 * s)
divv (Vector x1 y1) s = Vector (x1 / s) (y1 / s)
absv (Vector x y) = sqrt ((x * x) + (y * y))
invv (Vector x y) = Vector y (-x)

lineToV (Line a b) = subv b a
absl l = absv (lineToV l)

divl (Line (Vector x1 y1) (Vector x2 y2)) s = Line (Vector x1 y1) (addv (Vector x1 y1) (Vector ((x2 - x1) / s) ((y2 - y1) / s)))

lineToVs (Line v1 v2) = [v1, v2]
