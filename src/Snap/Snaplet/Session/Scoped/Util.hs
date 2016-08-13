module Snap.Snaplet.Session.Scoped.Util
    ( SessionState(..), getSessionDuration, newExpirationDate
    ) where


import           Data.Time.Clock
import Snap
import Control.Lens


data SessionState a
    = NotLoaded
    | Loaded ByteString a
    | Changed Bool Bool ByteString a


getSessionDuration :: Handler v b NominalDiffTime
getSessionDuration = do
    conf <- getSnapletUserConfig
    liftIO $ fromInteger . (*60) . fromMaybe (60*24) <$> C.lookup conf "session-duration"


newExpirationDate :: Handler v b UTCTime
newExpirationDate = addUTCTime <$> getSessionDuration <*> liftIO getCurrentTime


getSessionCookie :: Handler v b ByteString
getSessionCookie = do
    conf <- getSnapletUserConfig
    cookieName <- liftIO $ fromMaybe "sess" <$> C.lookup conf "cookie-name"
    getCookie cookieName


genNewToken :: (Lens' b RNG) -> (ByteString -> IO Bool) -> Handler v b ByteString
genNewToken rng test = do
    conf <- getSnapletUserConfig
    len <- liftIO $ fromMaybe 20 <$> C.lookup conf "token-length"
    newCookie <- liftIO mkNewToken
    myRNG <- getsSnapletState (^.rng)
    let mkNewToken = do
            newToken <- randomToken len myRNG
            cond <- test newToken
            if cond
                then mkNewToken
                else return newToken
    mkNewToken
