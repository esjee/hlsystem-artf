module Turtle where

import Graphics.Rendering.OpenGL

data Turtle = Turtle {
	xpos :: GLfloat,
	ypos :: GLfloat,
	angle :: GLfloat
} deriving (Show)

turtleLeft :: Turtle -> GLfloat -> Turtle
turtleLeft (Turtle x y dir) (angle) = Turtle x y (dir + angle)

turtleRight :: Turtle -> GLfloat -> Turtle
turtleRight (Turtle x y dir) (angle) = Turtle x y (dir - angle)

turtleForward :: Turtle -> GLfloat -> Turtle
turtleForward (Turtle x y dir) (dist) = Turtle (x+dx) (y+dy) dir where
	dx = dist * cos(pi * dir / 180)
	dy = dist * sin(pi * dir / 180)

turtleBack:: Turtle -> GLfloat -> Turtle
turtleBack (Turtle x y dir) (dist) = turtleForward (Turtle x y dir) (-dist)
