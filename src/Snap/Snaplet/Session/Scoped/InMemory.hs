{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
module Snap.Snaplet.Session.Scoped.InMemory
    ( initMemoryManager, MemoryManager
    ) where


import           ClassyPrelude
import           Control.Lens
import           Control.Monad.State.Class
import           Data.IORef                  as IORef
import           Snap
import           Snap.Snaplet.Session.Common
import           Snap.Snaplet.Session.Scoped hiding (loadSession)


data SessionState a
    = NotLoaded
    | Loaded ByteString a
    | Changed Bool Bool ByteString a


-- | A Manager for your session using a cookie + token on the client
-- and storing the session data serverside in memory
data MemoryManager a = MkMemoryManager
    { _globalState       :: IORef (HashMap ByteString a)
    , _currentSession    :: SessionState a
    , _sessionCookieName :: ByteString
    , _tokenLength       :: Int
    , _initialSession    :: a
    , _rng               :: RNG
    }


-- | Uses this function to create a new 'MemoryManager' in your snaplet initializer
initMemoryManager :: MonadIO m
                  => Maybe ByteString -- ^ A name for the session cookie (default "sess")
                  -> Maybe Int -- ^ A length (in characters) for the session token
                  -> a -- ^ initial state value for a new session
                  -> m (MemoryManager a)
initMemoryManager name len initial = do
    ref <- liftIO $ IORef.newIORef mempty
    gen <- liftIO mkRNG
    return $ MkMemoryManager ref NotLoaded (fromMaybe "sess" name) (fromMaybe 20 len) initial gen


makeLenses ''MemoryManager


loadSession :: (MonadSnap m, MonadState (MemoryManager a) m) => m (ByteString, a)
loadSession = do
    man <- get

    case man^.currentSession of
        Loaded token data_ -> return (token, data_)
        Changed _ _ token data_ -> return (token, data_)
        NotLoaded -> do
            cookie <- getCookie (man^.sessionCookieName)
            pl <- liftIO $ IORef.readIORef ref
            maybe
                genNew
                (\Cookie{ cookieValue = oldCookie } -> do
                    logError $  "Cookie is " ++ oldCookie
                    maybe
                        genNew
                        (\oldVal -> do
                            currentSession .= Loaded oldCookie oldVal
                            return (oldCookie, oldVal))
                        $ pl^?ix oldCookie)
                cookie
          where
            ref = man^.globalState
            mkNewToken = do
                newToken <- randomToken (man^.tokenLength) (man^.rng)
                pl <- IORef.readIORef ref
                if newToken `member` pl
                    then mkNewToken
                    else return newToken
            genNew = do
                newCookie <- liftIO mkNewToken
                currentSession .= Changed True True newCookie (man^.initialSession)
                return (newCookie, man^.initialSession)


instance Manager (MemoryManager a) where
    type ManagedState (MemoryManager a) = a

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
            Changed tokenChanged dataChanged token data_ -> do
                when tokenChanged $ modifyResponse (addResponseCookie $ Cookie (man^.sessionCookieName) token Nothing Nothing (Just "/") False False)
                when dataChanged $ liftIO $ IORef.modifyIORef (man^.globalState) $ at token .~ Just data_
                currentSession .= NotLoaded
            NotLoaded -> return ()
            _ -> return ()
