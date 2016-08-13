{-|
Module      : $Header$
Description : Concrete in-memory session state manager
Copyright   : (c) Justus Adam, 2016
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX
-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TemplateHaskell  #-}
{-# LANGUAGE TypeFamilies     #-}
module Snap.Snaplet.Session.Scoped.InMemory
    ( initMemoryManager, initMemoryManager', MemoryManager
    ) where


import           ClassyPrelude
import           Control.Concurrent.Async
import           Control.Concurrent.MVar     as MVar
import           Control.Lens
import           Control.Monad.State.Class
import qualified Data.Configurator           as C
import           Data.Default
import qualified Data.HashMap.Strict         as HashMap
import           Data.IORef                  as IORef
import           Data.Time.Clock
import           Snap
import           Snap.Snaplet.Session.Common
import           Snap.Snaplet.Session.Scoped hiding (loadSession)


-- | A Manager for your session using a cookie + token on the client
-- and storing the session data serverside in memory
data MemoryManager a = MkMemoryManager
    { _globalState    :: MVar (HashMap ByteString (UTCTime, a))
    , _currentSession :: SessionState a
    , _initialSession :: a
    , _rng            :: RNG
    , _janitorWorker  :: IORef (Async ())
    }


runJanitor :: MVar (HashMap ByteString (UTCTime, a)) -> Int -> IO ()
runJanitor ref delay = forever $ do
    threadDelay $ 60000000 * delay
    t <- getCurrentTime
    MVar.modifyMVar_ ref $ return . HashMap.filter ((< t) . fst)


-- | Convenience function for creating managers for values with defined defaults
initMemoryManager :: (Default a, MonadIO (m v b), MonadSnaplet m) => m v b (MemoryManager a)
initMemoryManager = initMemoryManager' def


-- | Uses this function to create a new 'MemoryManager' in your snaplet initializer
initMemoryManager' :: (MonadIO (m v b), MonadSnaplet m)
                   => a -- ^ initial state value for a new session
                   -> m v b (MemoryManager a)
initMemoryManager' initial = do
    ref <- liftIO $ MVar.newMVar mempty
    gen <- liftIO mkRNG
    conf <- getSnapletUserConfig
    delay <- liftIO $ fromMaybe (60*12) <$> C.lookup conf "worker-delay"
    janitor <- liftIO $ async $ runJanitor ref delay
    janRef <- liftIO $ IORef.newIORef janitor
    return $ MkMemoryManager ref NotLoaded initial gen janRef


makeLenses ''MemoryManager


loadSession :: Handler b (MemoryManager a) (ByteString, a)
loadSession = do
    man <- get

    case man^.currentSession of
        Loaded token data_ -> return (token, data_)
        Changed _ _ token data_ -> return (token, data_)
        NotLoaded -> do
            conf <- getSnapletUserConfig
            cookieName <- liftIO $ fromMaybe "sess" <$> C.lookup conf "cookie-name"
            cookie <- getCookie cookieName
            pl <- liftIO $ MVar.readMVar ref
            maybe
                genNew
                (\Cookie{ cookieValue = oldCookie } -> do
                    logError $  "Cookie is " ++ oldCookie
                    maybe
                        genNew
                        (\(_, oldVal) -> do
                            currentSession .= Loaded oldCookie oldVal
                            return (oldCookie, oldVal))
                        $ pl^?ix oldCookie)
                cookie
          where
            ref = man^.globalState
            mkNewToken len = do
                newToken <- randomToken len (man^.rng)
                pl <- MVar.readMVar ref
                if newToken `member` pl
                    then mkNewToken len
                    else return newToken
            genNew = do
                conf <- getSnapletUserConfig
                len <- liftIO $ fromMaybe 20 <$> C.lookup conf "token-length"
                newCookie <- liftIO $ mkNewToken len
                currentSession .= Changed True True newCookie (man^.initialSession)
                return (newCookie, man^.initialSession)


instance Manager (MemoryManager a) where
    type Manages (MemoryManager a) = a

    managerLoad = void loadSession
    managerGetSession = snd <$> loadSession

    managerModifySession f = do
        (token, data_) <- loadSession
        let changed = f data_
        man <- get
        let tokenHasChanged = case man^.currentSession of
                                Changed True _ _ _ -> True
                                _ -> False
        currentSession .= Changed tokenHasChanged True token changed
        return changed

    managerCommit = do
        man <- get
        case man^.currentSession of
            Changed tokenChanged _ token data_ -> do
                when tokenChanged $ do
                    conf <- getSnapletUserConfig
                    cookieName <- liftIO $ fromMaybe "sess" <$> C.lookup conf "cookie-name"
                    modifyResponse (addResponseCookie $ Cookie cookieName token Nothing Nothing (Just "/") False False)
                d <- newExpirationDate
                liftIO $ MVar.modifyMVar_ (man^.globalState) $ return . (at token .~ Just (d, data_))
                currentSession .= NotLoaded
            NotLoaded -> return ()
            Loaded token _ -> do
                d <- newExpirationDate
                liftIO $ MVar.modifyMVar_ (man^.globalState) $ return . (ix token . _1 .~ d)
