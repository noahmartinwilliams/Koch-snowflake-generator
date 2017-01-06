module Snowflake where

import Vector


newLines :: Line -> [Line]
newLines (Line (Vector lx1 ly1) (Vector lx2 ly2)) = do
	let p = Vector lx1 ly1 in
		let l = Line (Vector lx1 ly1) (Vector lx2 ly2) in
			let v = Vector (ly2 - ly1) (- (lx2 - lx1)) in
				let v2 = mulv (divv v (absv v)) ((sin (60.0 * pi / 180.0)) * (1.0 / 3.0) * (absl l))  in
					let v3 = addv (addv v2 (divv (lineToV l) 2.0)) p in
						let nl1 = Line (addv (divv (lineToV l) 3.0) p) v3 in
							let nl2 = Line v3 (addv (mulv (divv (lineToV l) 3.0) 2.0) p) in
								[nl1, nl2, (Line p (addv (lineToV (divl l 3.0)) p)), (Line (addv (mulv (lineToV (divl l 3.0)) 2.0) p) (Vector lx2 ly2))]

iterateSnowflake :: [Line] -> [Line]
iterateSnowflake list = foldr (++) [] (map newLines list)

iterateTil :: Double -> [Line] -> [Line]
iterateTil s lines = let l = (iterateSnowflake lines)  in
	if (takeWhile (\a -> (absl a) >= s) l) /= []
	then
		l ++ lines ++ (iterateTil s l)
	else
		[]

apply :: (a -> b) -> [a] -> [b]
apply _ [] = []
apply f (first : rest) = (f first) : (apply f rest)

initialLines :: [Line]
initialLines = [(Line (Vector (-0.5) 0.0) (Vector 0.5 0.0)), (Line (Vector 0.5 0.0) (Vector 0.0 (sin (60.0 * pi / 180.0)))), (Line  (Vector 0.0 (sin (60.0 * pi / 180.0))) (Vector (-0.5) 0.0))]
