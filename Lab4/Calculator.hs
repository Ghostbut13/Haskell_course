-- This module is a starting point for implementing the Graph Drawing
-- Calculator as described in Part II of the Standard Lab. You can use this
-- directly, or just study it as an example of how to use threepenny-gui.
module Calculator where

import Expr  -- Importing Expr module from Part I
import ThreepennyPages
import Graphics.UI.Threepenny.Core as UI
import qualified Graphics.UI.Threepenny as UI

canWidth,canHeight :: Num a => a
canWidth  = 300
canHeight = 300

main :: IO ()
main = startGUI defaultConfig setup

setup :: Window -> UI ()
setup window =
  do -- Create them user interface elements
     canvas  <- mkCanvas canWidth canHeight   -- The drawing area
     fx      <- mkHTML "<i>f</i>(<i>x</i>)="  -- The text "f(x)="
     input   <- mkInput 20 "x"                -- The formula input
     draw    <- mkButton "Draw graph"         -- The draw button
    -- The markup "<i>...</i>" means that the text inside should be rendered
    -- in italics.

    -- Add the user interface elements to the page, creating a specific layout
     formula <- row [pure fx,pure input]
     getBody window #+ [column [pure canvas,pure formula,pure draw]]

     -- Styling
     getBody window # set style [("backgroundColor","lightblue"),
                                 ("textAlign","center")]
     pure input # set style [("fontSize","14pt")]

     -- Interaction (install event handlers)
     on UI.click     draw  $ \ _ -> readAndDraw input canvas
     on valueChange' input $ \ _ -> readAndDraw input canvas


readAndDraw :: Element -> Canvas -> UI ()
readAndDraw input canvas =
  do -- Get the current formula (a String) from the input element
     formula <- get value input
     -- Clear the canvas
     clearCanvas canvas
     -- The following code draws the formula text in the canvas and a blue line
     -- It should be replaced with code that draws the graph of the function.
     set UI.fillStyle (UI.solidColor (UI.RGB 0 0 0)) (pure canvas)
     UI.fillText formula (10,canHeight/2) canvas
     path "blue" [(10,10),(canWidth-10,canHeight/2)] canvas

----H: Function to generate points for the graph based on an expression
type Point = (Double, Double)
points :: Expr -> Double -> (Int, Int) -> [Point]
points expr scale (width, height) = 
 --Generates points for the graph 
  map (\x -> (x, realToPix $ eval expr $ pixToReal x)) [0 .. fromIntegral width] 
  
  where
    halfWidth = fromIntegral width / 2
    halfHeight = fromIntegral height / 2
    pixToReal :: Double -> Double
    pixToReal x = (x - halfWidth) * scale
    realToPix :: Double -> Double
    realToPix y = halfHeight - (y / scale)

----I: Updated readAndDraw function to draw the actual graph
readAndDraw :: Element -> Canvas -> UI ()
readAndDraw inputField canvas = do
    -- Retrieves the expression string from the input field
    exprStr <- get value inputField 
    
    case readExpr exprStr of
        Just expr -> do
            let graphPoints = points expr 0.04 (300, 300)
            canvas # UI.clearCanvas
            -- Draws the graph using the calculated points
            canvas # drawGraph graphPoints 
            -- If parsing fails, clears the canvas
        Nothing -> canvas # UI.clearCanvas 
drawGraph :: [Point] -> Canvas -> UI ()
drawGraph points canvas = do
    canvas # UI.beginPath
    canvas # UI.moveTo (head points)
    mapM_ (canvas # UI.lineTo) (tail points)
    canvas # UI.stroke

----J: Function to adjust graph scale and redraw
adjustScaleAndRedraw :: Element -> Element -> Canvas -> UI ()
adjustScaleAndRedraw inputField scaleInput canvas = do 
    -- Takes input field, scale input, and canvas elements
    -- Retrieves the expression string from the input field
    -- Retrieves the scale value as a string from the scale input
    -- Default scale is 0.04
    exprStr <- get value inputField 
    scaleStr <- get value scaleInput 
    let scale = maybe 0.04 read (readMaybe scaleStr :: Maybe Double) 
    case readExpr exprStr of
        Just expr -> do
            let graphPoints = points expr scale (300, 300) 
            canvas # UI.clearCanvas
            drawGraph graphPoints canvas
        Nothing -> canvas # UI.clearCanvas 

----K: Function to differentiate expression and update graph
differentiateAndDraw :: Element -> Canvas -> UI ()
differentiateAndDraw inputField canvas = do
    exprStr <- get value inputField
    case readExpr exprStr of
        Just expr -> do
            let diffExpr = differentiate expr
            -- Updates the input field with the differentiated expression
            element inputField # set value (showExpr diffExpr) 
            let graphPoints = points diffExpr 0.04 (300, 300) 
            canvas # UI.clearCanvas
            drawGraph graphPoints canvas
        Nothing -> canvas # UI.clearCanvas 
