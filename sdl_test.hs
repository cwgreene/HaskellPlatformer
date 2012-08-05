import Control.Monad
import System.Time
import Data.Word

import Graphics.UI.SDL as SDL
import Graphics.UI.SDL.Time as SDLTime

loadImage :: String -> IO Surface
loadImage filename = loadBMP filename >>= displayFormat

applySurface :: Int -> Int -> Surface -> Surface -> IO Bool
applySurface x y src dst = blitSurface src Nothing dst offset
	where offset = Just Rect { rectX = x, rectY = y, rectW = 0, rectH = 0}

data Signal = SOther | SQuit 
		| SMoveDown 
		| SMoveUp
		| SMoveLeft
		| SMoveRight deriving Eq
data Position = Position Int Int
data Velocity = Velocity Int Int
data Sprite = PositionSprite Position Velocity Surface
data State = GameState [Sprite] Word32

spriteX (PositionSprite (Position x y) _ _) = x
spriteY (PositionSprite (Position x y) _ _) = y

driftSprite :: Sprite -> Sprite
driftSprite (PositionSprite (Position x y) (Velocity vx vy) s) = 
	(PositionSprite 
	(Position (x+vx) (y+vy) )
	(Velocity vx vy)
	s)

driftSprites :: [Sprite] -> [Sprite]
driftSprites sprites = map driftSprite sprites

updateGameState :: State ->  Word32 -> State
updateGameState (GameState sprites time) update = (GameState (driftSprites sprites) update)

accelerateSprite (PositionSprite pos (Velocity vx vy) s) dvx dvy = 
	(PositionSprite
	 pos
	 (Velocity (vx+dvx) (vy+dvy))
	 s)

acceleratePlayer (GameState sprites time) x y = GameState [(accelerateSprite (head sprites)) x y] time

next_state :: State -> Signal -> State
next_state state signal = case signal of 
	SMoveDown -> acceleratePlayer state 0 1
	SMoveUp -> acceleratePlayer state 0 (-1)
	SMoveLeft -> acceleratePlayer state (-1) 0
	SMoveRight -> acceleratePlayer state 1 0
	_ -> state
			

draw_sprite :: Sprite -> Surface -> IO Bool
draw_sprite (PositionSprite (Position x y) v surface) screen = applySurface x y surface screen

draw_state :: State -> Surface -> IO Bool
draw_state (GameState [] time) screen = return True
draw_state (GameState (sprite:xs) time) screen = do 
	draw_sprite sprite screen
	draw_state (GameState xs time) screen

handleKey :: Keysym -> Signal
handleKey (Keysym key _ _) = case key of
				SDLK_a -> SMoveLeft
				SDLK_d -> SMoveRight
				SDLK_w -> SMoveUp
				SDLK_s -> SMoveDown
				_ -> SOther

handle_signal :: State -> Signal -> Surface -> IO ()
handle_signal state signal screen =  do 
	draw_state next screen
	SDL.flip screen
	loop next screen
	where
		next = next_state state signal

whileEvents :: IO Signal
whileEvents = do
	event <- pollEvent
	case event of
		Quit -> return SQuit
		NoEvent -> return SOther
		KeyUp akey -> return (handleKey akey)
		_ -> whileEvents

needUpdate (GameState sprites last_update) cur_time = (cur_time - last_update) > 100

loop :: State -> Surface -> IO ()
loop state screen = do
		signal <- whileEvents
		cur_time <- getTicks
		unless (signal == SQuit) (if (needUpdate state cur_time)
			then (handle_signal (updateGameState state cur_time) signal screen)
			else (handle_signal state signal screen))
main = withInit [InitEverything] $ do
	screen <- setVideoMode screenWidth screenHeight screenBpp [SWSurface]
	setCaption "Haskell SDL" []

	message <- loadImage "hello.bmp"

	SDL.flip screen
	start_time <- getTicks

	let state = GameState [(PositionSprite (Position 180 100) (Velocity 0 0) message)] start_time in 
		loop state screen
  where
	screenWidth = 640
	screenHeight = 480
	screenBpp = 32
