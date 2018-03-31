import qualified GI.Gtk as Gtk
import GI.Gtk hiding (main)
import GI.Gtk.Enums (WindowType(..))

main = do
    _ <- Gtk.init Nothing

    window <- windowNew WindowTypeToplevel
    _ <- onWidgetDestroy window mainQuit
    setContainerBorderWidth window 10
    setWindowTitle window "Hello World"

    button <- buttonNew
    setButtonLabel button "Hello World"
    _ <- onButtonClicked button $
        putStrLn "Hello World"

    setContainerChild window button
    widgetShowAll window

    Gtk.main
