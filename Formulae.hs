module Formulae
(
 Formula,
 vertices,
 vertType,
 emptyFormula,
 sierpinski, cantor, koch,
 dragon,
 plant,
 ) where

import Graphics.Rendering.OpenGL
import Turtle

-- Every 'formula' has the following properties (no more, no less)
--	Amount of generations (n)
--	Line length (l) distance taken by turtle when moving 1 unit
--	Angle (l) not yet implemented, but yeah, it's the angle for the turtle
--
-- _*F methods generated a code (n lineLength Angle -> code)
-- _*V methods generated Vertices (code -> formula)
-- The public methods simply wrap these two up for a clean interface

data Formula = Formula {
		vertices :: [[Vertex2 GLfloat]],
		vertType :: PrimitiveMode
} deriving (Show)

emptyFormula :: Int -> GLfloat -> Formula
emptyFormula _ _ = Formula [[]] LineStrip

-- Sierpinski
sierpinski :: Int -> GLfloat -> GLfloat -> Formula
sierpinski n l a = Formula [vertices] LineStrip
	where
		vertices = _sierpinskiV (Turtle 0 0 0) (_sierpinskiF n) l a

_sierpinskiF :: Int -> [Char]
_sierpinskiF n
	| n > 0 = concat $ map (sub) (_sierpinskiF(n-1))
	| n == 0 = ['A']
	where
		sub :: Char -> [Char]
		sub a
			| a == 'A' = "B-A-B"
			| a == 'B' = "A+B+A"
			| a == '+' = "+"
			| a == '-' = "-"

_sierpinskiV :: Turtle -> [Char] -> GLfloat -> GLfloat -> [Vertex2 GLfloat]
_sierpinskiV _ [] _ _ = []
_sierpinskiV turtle@(Turtle x y dir) (f:fs) l a
	| f == 'A' || f == 'B' =
		Vertex2 x y : _sierpinskiV (turtleForward turtle l) fs l a
	| f == '-' =
		_sierpinskiV (turtleLeft turtle a) fs l a
	| f == '+' =
		_sierpinskiV (turtleRight turtle a) fs l a

-- Cantor dust
-- Start: A
-- Rules: (A -> ABA) (B -> BBB)
-- A :: Draw forward
-- B :: Move forward

cantor :: Int -> GLfloat -> GLfloat -> Formula
cantor n l a = Formula [vertices] Lines
	where
		vertices = _cantorV (Turtle 0 0 0) (_cantorF n) l a

_cantorF :: Int -> [Char]
_cantorF n
	| n > 0 = concat $ map (sub) (_cantorF(n-1))
	| n == 0 = "A"
	where
		sub :: Char -> [Char]
		sub 'A' = "ABA"
		sub 'B' = "BBB"

_cantorV :: Turtle -> [Char] -> GLfloat -> GLfloat -> [Vertex2 GLfloat]
_cantorV _ [] _ _ = []
_cantorV turtle@(Turtle x y dir) (f:fs) l a
	| f == 'A' = _cantorV nturtle fs l a
	| f == 'B' = Vertex2 x y : Vertex2 nx ny : _cantorV nturtle fs l a
	where
		nturtle = turtleForward turtle l
		nx = xpos nturtle
		ny = ypos nturtle

-- Koch curve
-- Start: F
-- Rules: (F -> F+F-F-F+F)
-- F :: draw forward
-- + :: turn left
-- - :: turn right

koch :: Int -> GLfloat -> GLfloat -> Formula
koch n l a = Formula [vertices] LineStrip
	where
		vertices = _kochV (Turtle 0 0 0) (_kochF n) l a

_kochF :: Int -> [Char]
_kochF 0 = "F"
_kochF n = concat $ map (sub) (_kochF (n-1))
	where
		sub :: Char -> [Char]
		sub 'F' = "F+F-F-F+F"
		sub c = c : ""

_kochV :: Turtle -> [Char] -> GLfloat -> GLfloat -> [Vertex2 GLfloat]
_kochV _ [] _ _ = []
_kochV turtle@(Turtle x y dir) (f:fs) l a
	| f == 'F' = Vertex2 x y : _kochV (turtleForward turtle l) fs l a
	| f == '+' = _kochV (turtleLeft turtle a) fs l a
	| f == '-' = _kochV (turtleRight turtle a) fs l a

-- Dragon curve
-- Start: FX
-- Rules: (X -> X+YF) (Y -> FX-Y)
-- F :: draw forward
-- - :: turn left
-- + :: turn right
-- X, Y :: do nothing

dragon :: Int -> GLfloat -> GLfloat -> Formula
dragon n l a = Formula [vertices] LineStrip
	where
		vertices = _dragonV (Turtle 0 0 0) (_dragonF n) l a

_dragonF :: Int -> [Char]
_dragonF 0 = "FX"
_dragonF n = concat $ map (sub) (_dragonF (n-1))
	where
		sub :: Char -> [Char]
		sub 'X' = "X+YF"
		sub 'Y' = "FX-Y"
		sub c = c : ""

_dragonV :: Turtle -> [Char] -> GLfloat -> GLfloat -> [Vertex2 GLfloat]
_dragonV _ [] _ _ = []
_dragonV turtle@(Turtle x y dir) (f:fs) l a
	| f == 'F' = Vertex2 x y : _dragonV (turtleForward turtle l) fs l a
	| f == '-' = _dragonV (turtleLeft turtle a) fs l a
	| f == '+' = _dragonV (turtleRight turtle a) fs l a
	| otherwise = _dragonV turtle fs l a

-- Fractal plant
-- Start: X
-- Rules: (X -> F-[x]+x]+F[+FX]-X) (F->FF)
-- Symbols:
-- F :: draw forward
-- - :: turn left
-- + :: turn right
-- X :: do nothing
-- [ :: save current position and angle
-- ] :: restore position and angle
plant :: Int -> GLfloat -> GLfloat -> Formula
plant n l a = Formula [vertices] Lines
	where
		vertices = _plantV (Turtle 0 0 0) (_plantF n) l a []

_plantF :: Int -> [Char]
_plantF 0 = "X"
_plantF n = concat $ map (sub) (_plantF (n-1))
	where
		sub :: Char -> [Char]
		sub 'X' = "F-[[X]+X]+F[+FX]-X"
		sub 'F' = "FF"
		sub n = n:""

_plantV :: Turtle -> [Char] -> GLfloat -> GLfloat -> [Turtle] -> [Vertex2 GLfloat]
_plantV _ [] _ _ _ = []
_plantV turtle@(Turtle x y dir) (f:fs) l a stack
	| f == 'F' = Vertex2 x y : _plantV (turtleForward turtle l) fs l a stack
	| f == '-' = _plantV (turtleLeft turtle a) fs l a stack
	| f == '+' = _plantV (turtleRight turtle a) fs l a stack
	| f == '[' = _plantV turtle fs l a (turtle:stack)
	| f == ']' = _plantV (head stack) fs l a (tail stack)
	| otherwise = _plantV turtle fs l a stack
