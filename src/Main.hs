import Control.Monad.Trans.Reader (runReaderT)
import Foreign.Ptr (castPtr)
import qualified GI.Gtk as Gtk
import GI.Gtk hiding (main)
import GI.Gtk.Enums (WindowType(..))
import qualified GI.Cairo
import Graphics.Rendering.Cairo hiding (x, y, width, height)
import qualified Graphics.Rendering.Cairo as Cairo
import Graphics.Rendering.Cairo.Internal (Render(runRender))
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))

data PlayerColor = Black | White deriving (Show, Eq)

main :: IO ()
main = do
    _ <- Gtk.init Nothing

    window <- windowNew WindowTypeToplevel
    _ <- onWidgetDestroy window mainQuit
    setContainerBorderWidth window 10
    setWindowTitle window "Hello World"

    -- button <- buttonNew
    -- setButtonLabel button "Hello World"
    -- _ <- onButtonClicked button $
    --     putStrLn "Hello World"

    boardDrawingArea <- drawingAreaNew
    widgetSetSizeRequest boardDrawingArea 400 400
    _ <- onWidgetDraw boardDrawingArea $ \context -> do
        renderWithContext context (drawBoard 9 boardDrawingArea)
        return True

    boardAspectFrame <- aspectFrameNew Nothing 0.5 0.5 1 False
    setContainerChild boardAspectFrame boardDrawingArea

    setContainerChild window boardAspectFrame
    widgetShowAll window

    Gtk.main

renderWithContext :: GI.Cairo.Context -> Render () -> IO ()
renderWithContext context r =
    withManagedPtr context $ \p ->
    runReaderT (runRender r) (Cairo (castPtr p))

starPoints :: Int -> [(Double, Double)]
starPoints 9 = (400, 400) : [(x, y) | x <- [200, 600], y <- [200, 600]]
starPoints 13 = (600, 600) : [(x, y) | x <- [300, 900], y <- [300, 900]]
starPoints 19 =  [(x, y) | x <- [300, 900, 1500], y <- [300, 900, 1500]]
starPoints _ =  []

drawBoard :: Int -> DrawingArea -> Render ()
drawBoard size boardDrawingArea = do
    --  lrc = lower-right coordinate
    let lrc = (fromIntegral size - 1) * 100
        boardMargin = 55
    width <- liftIO $ fromIntegral <$> widgetGetAllocatedWidth boardDrawingArea
    height <- liftIO $ fromIntegral <$> widgetGetAllocatedHeight boardDrawingArea
    scale (width / (lrc + (2 * boardMargin))) (height / (lrc + (2 * boardMargin)))
    translate boardMargin boardMargin
    setSourceRGB 0.8 0.65 0.32
    paint
    setSourceRGB 0.2 0.2 0.2
    mapM_ (\(a, b, c, d) -> line a b c d) [(x, 0, x, lrc) | x <- [0, 100 .. lrc]]
    mapM_ (\(a, b, c, d) -> line a b c d) [(0, y, lrc, y) | y <- [0, 100 .. lrc]]
    mapM_ (\(x, y) -> drawCircle x y 12) (starPoints size)
    drawStone Black 3 3
    drawStone White 3 4

drawStone :: PlayerColor -> Int -> Int -> Render ()
drawStone color x y = do
    setPlayerColor color
    drawCircle (fromIntegral x * 100) (fromIntegral y * 100) 47

drawCircle :: Double -> Double -> Double -> Render ()
drawCircle x y r = arc x y r 0 (1.999999 * pi) >> closePath >> fill

setPlayerColor :: PlayerColor -> Render ()
setPlayerColor Black = setSourceRGB 0 0 0
setPlayerColor White = setSourceRGB 1 1 1

line :: Double -> Double -> Double -> Double -> Render ()
line startX startY endX endY = do
    moveTo startX startY
    lineTo endX endY
    setLineWidth 3
    setLineJoin LineJoinRound
    setLineCap LineCapRound
    stroke
