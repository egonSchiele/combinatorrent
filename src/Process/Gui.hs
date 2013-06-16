{-# LANGUAGE OverloadedStrings #-}

module Process.Gui where
import Graphics.Vty hiding (Button)
import Graphics.Vty.Widgets.All
import qualified Data.Text as T
import Control.Monad
import System.IO
import Control.Applicative
import Control.Monad.IO.Class
import Control.Concurrent
import Control.Concurrent.STM
import Process
import qualified Process.Status as St
import qualified Data.ByteString.Char8 as B
import Supervisor
import Control.Monad.Reader
import System.Exit ( exitSuccess )

instance Logging (TChan a) where
    logName _ = "Process.Gui"

confirmation :: T.Text -> IO () -> IO () -> IO ()
confirmation str accept cancel = do
  u <- plainText str

  pe <- padded u (padLeftRight 2)
  (d, dFg) <- newDialog pe str
  setNormalAttribute d (white `on` blue)

  c <- centered =<< withPadding (padLeftRight 10) (dialogWidget d)

  coll <- newCollection
  addToCollection coll c dFg

  d `onDialogAccept` const (shutdownUi >> accept)
  d `onDialogCancel` const (shutdownUi >> cancel)

  runUi coll $ defaultContext { focusAttr = black `on` yellow }

for = flip map

-- foldBars :: (Show a, Show b) => [Widget ProgressBar] -> IO (Widget (Box a b))
-- foldBars (x:[]) = (return x) <--> plainText "ok!"
-- foldBars (x:xs) = (return x) <--> (foldBars xs)

start :: TMVar () -> St.StatusChannel -> [String] -> SupervisorChannel -> IO ThreadId
start waitC statusC names supC = do
    wrtC <- newTChanIO
    spawnP (wrtC) () ({-# SCC "Console" #-}
                                catchP (mainScreen waitC statusC names supC) (defaultStopHandler supC))

mainScreen :: TMVar () -> St.StatusChannel -> [String] -> SupervisorChannel -> Process (TChan String) b ()
mainScreen waitC statusC names supC = do
  wrtC <- ask
  liftIO $ do
    let completeAttr   = white `on` red
        incompleteAttr = red `on` white

    statusBars <- forM names $ \_ -> newProgressBar completeAttr incompleteAttr

    -- status <- plainText "status"

    fg <- newFocusGroup

    c <- centered =<< do
        plainText "Your torrents (press ESC to quit):"
          <--> (return $ head statusBars) -- fuck THIS BULLSHIT. HOW THE FUCK DO I FOLD ON <-->!!!
          >>= withBoxSpacing 1

    coll <- newCollection
    addToCollection coll c fg

    let quitFunc = do
          shutdownUi
          atomically $ putTMVar waitC ()

    let refreshProgress = do
          v <- newEmptyTMVarIO
          atomically $ writeTChan statusC (St.RequestAllTorrents v)
          sts <- atomically $ takeTMVar v
          forM_ (zip3 sts statusBars names) $ \((infoHash, state), bar, name) -> do
            let downloaded = fromIntegral . St.downloaded $ state
                left       = fromIntegral . St.left       $ state
                percent    = ((downloaded * 100) `div` (downloaded + left))
            setProgressText bar (T.pack $ name ++ " (" ++ (show percent) ++ "%)")
            setProgress bar percent

    forkIO $ do
      forever $ do
        m <- atomically $ readTChan wrtC
        putStrLn (m ++ " !!")

    forkIO $ do
      forever $ do
        threadDelay (100000)
        schedule refreshProgress

    fg `onKeyPressed` \_ k _ ->
        case k of
          KEsc -> do
            confirmation "Do you really want to quit?" quitFunc (return ())
            return True
          _ -> return False

    runUi coll $ defaultContext { focusAttr = fgColor blue }
