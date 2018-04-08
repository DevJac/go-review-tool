import Control.Monad.Trans.Reader (runReaderT)
import Foreign.Ptr (castPtr)
import qualified GI.Gtk as Gtk
import GI.Gtk hiding (main)
import GI.Gtk.Enums (WindowType(..))
import qualified GI.Cairo
import Graphics.Rendering.Cairo
import Graphics.Rendering.Cairo.Internal (Render(runRender))
import Graphics.Rendering.Cairo.Types (Cairo(Cairo))

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
        renderWithContext context $ do
            setSourceRGB 0 2000 0
            paint
        return True


    setContainerChild window boardDrawingArea
    widgetShowAll window

    Gtk.main

renderWithContext :: GI.Cairo.Context -> Render () -> IO ()
renderWithContext context r =
    withManagedPtr context $ \p ->
    runReaderT (runRender r) (Cairo (castPtr p))
