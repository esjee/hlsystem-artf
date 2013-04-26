import Graphics.UI.SDL
import Graphics.Rendering.OpenGL
import Data.List
import Data.Maybe
import Data.Word

import Control.Monad
import Control.Monad.State

import Formulae

type ARTFVect = (GLfloat, GLfloat, GLfloat)

data Camera = Camera {
	pos :: ARTFVect
}

data ARTFFlags = Alive | Dead
	deriving (Eq)

data ARTF = ARTF {
	flags :: ARTFFlags,
	cam :: Camera,
	formula :: Int -> GLfloat -> GLfloat -> Formula,
	gen :: Int, -- change this to (Data.)Word
	lineLen :: GLfloat,
	angle :: GLfloat,
	userinput :: String
}

initialARTF :: ARTF
initialARTF = ARTF Alive (Camera (0.0, 0.0, 0.0)) plant 6 0.1 6 ""

handleUserInput :: ARTF -> ARTF
handleUserInput artf = artf

incGenerations :: Int -> ARTF -> ARTF
incGenerations nN artf@(ARTF flags cam f n l a ui)
	| newN > 0 = ARTF flags cam f newN l a ui
	| otherwise = artf
	where newN = nN + n

incAngle :: GLfloat -> ARTF -> ARTF
incAngle nA artf@(ARTF flags cam f n l a ui) = ARTF flags cam f n l (a+nA) ui

artfAlive :: ARTF -> Bool
artfAlive artf = (flags artf) == Alive

main :: IO ()
main = do
	Graphics.UI.SDL.init [InitVideo]
	setVideoMode 640 480 32 [OpenGL, HWSurface]
	viewport $= (Position 0 0, Size 640 480)
	clearColor $= (Color4 0 1.0 0 0)
	--initialDisplayMode $= [DoubleBuffered, RGBMode, WithDepthBuffer]
	lighting $= Enabled
	depthFunc $= Just Less

	scr <- getVideoSurface

	gameLoop initialARTF scr
	quit

gameLoop :: ARTF -> s -> IO ()
gameLoop artf@(ARTF flags (Camera (cx, cy, cz)) f n l a ui) w = do
	clear [ColorBuffer, DepthBuffer]
	loadIdentity
	translate $ Vector3 cx cy cz
	--rotate (90 :: GLfloat) $ Vector3 0 0 1

	mapM_ (renderPrimitive (vertType vert) . mapM_ vertex) (vertices vert)

	--mapM_ (renderPrimitive LineStrip . mapM_ vertex) (sierpinski 8 lineLen)

	translate $ Vector3 (-cx) (-cy) (-cz)

	glSwapBuffers

	events <- getEvents
	let newartf = handleEvents artf events

	if artfAlive newartf
	then gameLoop newartf w
	else return ()
	where
		vert = f n l a

-- Get all Events so they can be worked with in a pure context
getEvents :: IO [Event]
getEvents = do
	ev <- pollEvent
	nextEvents ev
	where
		nextEvents :: Event -> IO [Event]
		nextEvents NoEvent = return []
		nextEvents x = do
			ev <- pollEvent
			xs <- nextEvents ev
			return (x:xs)

-- a b are ignored
handleKeyDown :: ARTF -> SDLKey -> a -> b -> ARTF
handleKeyDown artf@(ARTF flags oldcam@(Camera (cx, cy, cz)) f n l a ui) key sym mod
	| key == SDLK_q = (ARTF Dead oldcam f n l a ui)
	| key == SDLK_d = (ARTF flags (Camera ((cx-0.1), cy, cz)) f n l a ui)
	| key == SDLK_a = (ARTF flags (Camera ((cx+0.1), cy, cz)) f n l a ui)
	| key == SDLK_w = (ARTF flags (Camera (cx, (cy-0.1), cz)) f n l a ui)
	| key == SDLK_s = (ARTF flags (Camera (cx, (cy+0.1), cz)) f n l a ui)
	| key == SDLK_v = (ARTF flags oldcam f n (l/2) a ui)
	| key == SDLK_b = (ARTF flags oldcam f n (l*2) a ui)
	| key == SDLK_y = (ARTF flags oldcam sierpinski n l a ui)
	| key == SDLK_t = (ARTF flags oldcam cantor n l a ui)
	| key == SDLK_f = (ARTF flags oldcam koch n l a ui)
	| key == SDLK_e = (ARTF flags oldcam dragon n l a ui)
	| key == SDLK_g = (ARTF flags oldcam plant 6 l 25 ui)
	| key == SDLK_p = incGenerations 1 artf
	| key == SDLK_o = incGenerations (-1) artf
	| key == SDLK_r = initialARTF
	| key == SDLK_l = incAngle 5.0 artf
	| key == SDLK_k = incAngle (-5.0) artf
	| otherwise = (ARTF flags oldcam f n l a ui)

handleEvents :: ARTF -> [Event] -> ARTF
handleEvents artf [] = artf
handleEvents oldartf@(ARTF flags cam f n l a ui) (x:xs) = do
	case x of
		(KeyDown (Keysym key a b)) -> handleKeyDown oldartf key a b
		Quit -> (ARTF Dead cam f n l a ui)
		_ -> (ARTF flags cam f n l a ui)
