import Control.Monad.Trans.Reader (runReaderT)
import Foreign.Ptr (castPtr)
import qualified GI.Gtk as Gtk
import GI.Gtk hiding (main)
import GI.Gtk.Enums (WindowType(..))
import qualified GI.Cairo
import Graphics.Rendering.Cairo hiding (x, y)
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
    _ <- onWidgetDraw boardDrawingArea $ \context -> do
        renderWithContext context drawBoard
        return True

    setContainerChild window boardDrawingArea
    widgetShowAll window

    Gtk.main

renderWithContext :: GI.Cairo.Context -> Render () -> IO ()
renderWithContext context r =
    withManagedPtr context $ \p ->
    runReaderT (runRender r) (Cairo (castPtr p))

drawBoard :: Render ()
drawBoard = do
    translate 100 100
    scale 0.4 0.4
    setSourceRGB 0.8 0.65 0.32
    paint
    setSourceRGB 0.2 0.2 0.2
    mapM_ (\(a, b, c, d) -> line a b c d) [(x, 0, x, 1800) | x <- [0, 100 .. 1800]]
    mapM_ (\(a, b, c, d) -> line a b c d) [(0, y, 1800, y) | y <- [0, 100 .. 1800]]
    drawStone Black 300 300
    drawStone White 300 400

drawStone :: PlayerColor -> Double -> Double -> Render ()
drawStone color x y = do
    arc x y 47 0 (1.999999 * pi)
    closePath
    setPlayerColor color
    fill

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
