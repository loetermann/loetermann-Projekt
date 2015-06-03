{-# LANGUAGE OverloadedStrings #-}

import Graphics.UI.Gtk
import Graphics.Rendering.Cairo
import Control.Monad
import Control.Monad.State as State
import Data.Word
import Data.Either
import Data.Attoparsec.Text
import Data.Text.Encoding
import qualified Data.ByteString.Char8 as B (unpack, readFile, ByteString)
import Control.Applicative
import System.Environment

type Program = [Instruction]
data Instruction = GoTo Double Double | TurnTo Int | LookAt Int Int | Forward Int | Back Int | Turn Int | Home | Die | Repeat Int Program | SetColor Int Int Int | Pen Bool deriving Show
data TurtleState = TurtleState { xPos :: Double, yPos :: Double, rot :: Int, color :: (Double, Double, Double), pen :: Bool, dead :: Bool, homeX :: Double, homeY :: Double }
type WindowDimension = (Int, Int)

initState (width, height) = TurtleState { xPos=0, yPos=0, rot=0, color=(0, 0, 0), pen=True, dead=False, homeX=fromIntegral width / 2, homeY=fromIntegral height / 2 }

main :: IO ()
main = do
    [path] <- getArgs
    file <- B.readFile path
    let (input, (width, height)) = case parse parseHeader (decodeUtf8 file) of
            Fail t _ _ -> (t, (400, 400))
            Partial _ -> error "Unexpected end of file (header)"
            Done t r -> (t, r)
    let prog = case parseOnly parseProgram input of
            Left a -> error a
            Right prog -> prog
    initGUI
    window <- windowNew
    set window [windowTitle := B.unpack "Turtle",
        windowDefaultWidth := width, windowDefaultHeight := height,
        containerBorderWidth := 0 ]
    frame <- frameNew
    containerAdd window frame
    canvas <- drawingAreaNew
    containerAdd frame canvas
    widgetModifyBg canvas StateNormal (Color 65535 65535 65535)
    widgetShowAll window 
    drawin <- widgetGetDrawWindow canvas
    onExpose canvas (\x -> do renderWithDrawable drawin (draw (width, height) prog); return True)
    onDestroy window mainQuit
    mainGUI

performIfNotDead :: Instruction -> StateT TurtleState Render ()
performIfNotDead inst = do s <- State.get
                           unless (dead s) (perform inst)

perform :: Instruction -> StateT TurtleState Render ()
perform (GoTo x y) = do s <- State.get
                        let (r, g, b) = color s
                        put $ s { xPos=x, yPos=y }
                        lift $ setSourceRGB r g b
                        if pen s then do
                            lift $ moveTo (xPos s) (yPos s)
                            lift $ lineTo x y
                            lift $ stroke
                        else do
                            lift $ moveTo x y
perform (Forward step) = do s <- State.get
                            let x = xPos s + cos (pi / 180 * (fromIntegral $ rot s)) * fromIntegral step
                            let y = yPos s + sin (pi / 180 * (fromIntegral $ rot s)) * fromIntegral step
                            perform (GoTo x y)
perform (Back step) = perform (Forward (-step))
perform (Turn angle) = do modify (\s -> s { rot = rot s + angle })
perform (TurnTo angle) = do modify (\s -> s { rot = angle })
perform (LookAt x y) = modify (\s -> s { rot = round(180 / pi * atan(((fromIntegral y - yPos s) / (fromIntegral x - xPos s))))})
perform Home = do modify (\s -> s {xPos=homeX s, yPos=homeY s})
perform (Repeat 0 _) = do return ()
perform (Repeat i prog) = do sequence_ $ map performIfNotDead prog; performIfNotDead (Repeat (i-1) prog);
perform (SetColor r g b) = do modify (\s -> s {color = (fromIntegral r / 255, fromIntegral g / 255, fromIntegral b / 255 )})
perform (Pen b) = do modify (\s -> s {pen = b})
perform Die = do modify (\s -> s {dead = True})

draw :: WindowDimension -> Program -> Render ()
draw dim prog = do
    setLineWidth 2
    setSourceRGB 1 1 0
    execStateT ( sequence_ $ map performIfNotDead prog ) (initState dim)
    return ()

---- Parser ------
parseHeader :: Parser WindowDimension
parseHeader = do string "#Window"; anySpace; width <- decimal; anySpace; char 'x'; anySpace; height <- decimal; anySpace; endOfLine;
                    return (width, height)


parseProgram :: Parser Program
parseProgram = manyTill parseInstruct endOfInput

parseInstruct :: Parser Instruction
parseInstruct = do 
                    anySpaceOrLine
                    instruct <- parseInstruction
                    anyEnd <?> "Expected ; oder newLine"
                    anySpaceOrLine
                    return instruct

parseInstruction :: Parser Instruction
parseInstruction = do string "GoTo"; anySpace; x <- decimal; anySpace; y <- decimal; return (GoTo (fromIntegral x) (fromIntegral y))
                    <|> do string "TurnTo"; anySpace; angle <- decimal; return (TurnTo angle)
                    <|> do string "LookAt"; anySpace; x <- decimal; anySpace; y <- decimal; return (LookAt x y)
                    <|> do string "Forward"; anySpace; step <- decimal; return (Forward step)
                    <|> do string "Back"; anySpace; step <- decimal; return (Back step)
                    <|> do string "Left"; anySpace; angle <- decimal; return (Turn (-angle))
                    <|> do string "Right"; anySpace; angle <- decimal; return (Turn angle)
                    <|> do string "Home"; return Home
                    <|> do string "Die"; return Die
                    <|> do string "SetColor"; anySpace; r <- decimal; anySpace; g <- decimal; anySpace; b <- decimal; return (SetColor r g b)
                    <|> do string "PenUp"; return (Pen False)
                    <|> do string "PenDown"; return (Pen True)
                    <|> do string "Repeat"; anySpace; n <- decimal; anySpaceOrLine; char '{'; anySpaceOrLine;
                            prog <- manyTill parseInstruct (char '}'); return (Repeat n prog)

anySpace :: Parser ()
anySpace = do char ' '; anySpace
            <|> do char '\t'; anySpace
            <|> return ()

anySpaceOrLine :: Parser ()
anySpaceOrLine = do char ' '; anySpaceOrLine
            <|> do char '\t'; anySpaceOrLine
            <|> do endOfLine; anySpaceOrLine
            <|> return ()

anyEnd :: Parser ()
anyEnd = do char ';'; return ()
            <|> endOfLine
            <|> endOfInput
