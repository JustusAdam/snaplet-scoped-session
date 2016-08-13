module Snap.Snaplet.Session.Scoped.FS
    ( initFSManager, FSManager
    ) where


import Snap
import ByteString
import Control.Lens


data FSManager a = FSManager
    { _serialize :: a -> IO ByteString
    , _deserialize :: ByteString -> IO a
    , _initialSession :: a
    }


makeLenses ''FSManager


instance Manager (FSManager a) where
    type Manages (FSManager a) = a

    managerLoad = do
