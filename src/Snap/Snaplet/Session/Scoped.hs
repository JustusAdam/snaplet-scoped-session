{-# LANGUAGE AllowAmbiguousTypes    #-}
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TemplateHaskell        #-}
{-# LANGUAGE TypeFamilies           #-}
module Snap.Snaplet.Session.Scoped
    ( HasManager, toManager, ManagedState
    , Manager, managerGetSession, managerSetSession, managerModifySession, managerCommit, managerLoad
    , AccessSession, GlobalStateType, StateType, accessSession, AccessSessionLens, mkAccessSessionLens
    , initSessionSnaplet
    , getSession, setSession, modifySession, loadSession, commitSession
    ) where


import           ClassyPrelude
import           Control.Lens
import           Control.Monad.State.Class
import           Data.Default
import           Snap


-- | An abstract type for a session manager
--
-- Managers are responsible for storing the session state across requests and
-- optionally persistent
--
-- It should be noted that although an implementation for 'managerCommit' and
-- 'managerLoad' is not required it is useful for efficiancy reasons.
--
-- The state of the manager is mutable within one request but reset for new requests.
-- Therefore managers whould use mutable or persistent data structures like 'IORef' internally.
class Manager manager where
    type ManagedState manager
    managerGetSession :: (MonadSnap m, MonadState manager m) => m (ManagedState manager)
    managerGetSession = managerModifySession id
    managerSetSession :: (MonadSnap m, MonadState manager m) => ManagedState manager -> m ()
    managerSetSession = managerModifySession . const >=> const (return ())
    managerCommit :: (MonadSnap m, MonadState manager m) => m ()
    managerCommit = return ()
    managerLoad :: (MonadSnap m, MonadState manager m) => m ()
    managerLoad = return ()

    managerModifySession :: (MonadSnap m, MonadState manager m) => (ManagedState manager -> ManagedState manager) -> m (ManagedState manager)
    managerModifySession f = managerGetSession >>= \sess -> managerSetSession (f sess) >> return sess

    {-# MINIMAL managerGetSession, managerSetSession | managerModifySession #-}


-- | Class providing access to a Snaplet managing session state
class Manager m => HasManager a m | a -> m where
    toManager :: SnapletLens (Snaplet a) m


-- | Type magic
type AccessSessionLens t a b = t -> Lens' a b


-- | You should use this function to create a 'AccessSessionLens'. It ignores
-- the 't' argument. The 't' ergument is only used to line up types.
mkAccessSessionLens :: Lens' a b -> AccessSessionLens t a b
mkAccessSessionLens lens _ = lens


-- | A reference to a local session state from the global session state
-- You have to implement this class for your subsnaplet to get access to a part of the global session state.
class AccessSession t where
    type GlobalStateType t
    type StateType t
    accessSession :: AccessSessionLens t (GlobalStateType t) (StateType t)


-- | Initialize a session managing Snaplet from a manager. For an example manager see
-- 'Snap.Snaplet.Session.Scoped.InMemory'
initSessionSnaplet :: Manager a => a -> SnapletInit b a
initSessionSnaplet man = makeSnaplet "session-manager" "manages typed sessions" Nothing $ return man


getFullSession :: forall s m v. (HasManager s m) => Handler s v (ManagedState m)
getFullSession = withTop' (toManager :: SnapletLens (Snaplet s) m) managerGetSession


-- | Tells the session state manager to load the session.
-- Users should not have to call this function
loadSession :: forall s m v. (HasManager s m) => Handler s v ()
loadSession = withTop' (toManager :: SnapletLens (Snaplet s) m) managerLoad


-- | Tells the session manager to persist any changes.
-- Should be called at the end of a request cycle.
commitSession :: forall s m v. (HasManager s m) => Handler s v ()
commitSession = withTop' (toManager :: SnapletLens (Snaplet s) m) managerCommit


-- | Obtain the local session for the current snaplet.
getSession :: forall s t m. (HasManager s m, AccessSession t, GlobalStateType t ~ ManagedState m) => Handler s t (StateType t)
getSession = do
    fs <- getFullSession
    return $ fs^.accessSession (undefined :: t)


setFullSession :: forall s m v. (HasManager s m) => ManagedState m -> Handler s v ()
setFullSession sess = withTop' (toManager :: SnapletLens (Snaplet s) m) $ managerSetSession sess

modifyFullSession :: forall s m v. (HasManager s m) => (ManagedState m -> ManagedState m) -> Handler s v (ManagedState m)
modifyFullSession f = withTop' (toManager :: SnapletLens (Snaplet s) m) $ managerModifySession f


-- | Set the local part of the session state to a new value
setSession :: forall s t m. (HasManager s m, AccessSession t, GlobalStateType t ~ ManagedState m) => StateType t -> Handler s t ()
setSession inner = modifyFullSession (accessSession (undefined :: t) .~ inner) >> return ()


-- | Modify the local session state with a function, returns the altered local state
--
-- @
--      setSession v = modifySession (const v)
--      getSession = modifySession id
-- @
modifySession :: forall s t m. (HasManager s m, AccessSession t, GlobalStateType t ~ ManagedState m) => (StateType t -> StateType t) -> Handler s t (ManagedState m)
modifySession f = modifyFullSession (accessSession (undefined :: t) %~ f)
