{-# LANGUAGE ExplicitForAll         #-}
{-# LANGUAGE FlexibleContexts       #-}
{-# LANGUAGE RankNTypes             #-}
{-# LANGUAGE ScopedTypeVariables    #-}
{-# LANGUAGE TypeFamilies           #-}
module Snap.Snaplet.Session.Scoped
    ( HasManager, ManagerFor, toManager, ManagedState
    , Manager, managerGetSession, managerSetSession, managerModifySession, managerCommit, managerLoad
    , AccessSession, GlobalStateType, StateType, accessSession, AccessSessionLens, mkAccessSessionLens
    , initSessionSnaplet
    , getSession, setSession, modifySession, loadSession, commitSession
    ) where


import           ClassyPrelude
import           Control.Lens
import           Control.Monad.State.Class
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
class Manager (ManagerFor a) => HasManager a where
    type ManagerFor a
    toManager :: SnapletLens (Snaplet a) (ManagerFor a)


-- | Type magic
type AccessSessionLens t a b = t -> Lens' a b


-- | You should use this function to create a 'AccessSessionLens'. It ignores
-- the 't' argument. The 't' ergument is only used to line up types.
mkAccessSessionLens :: Lens' a b -> AccessSessionLens t a b
mkAccessSessionLens = const


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


getFullSession :: forall s v. HasManager s => Handler s v (ManagedState (ManagerFor s))
getFullSession = withTop' (toManager :: SnapletLens (Snaplet s) (ManagerFor s)) managerGetSession


-- | Tells the session state manager to load the session.
-- Users should not have to call this function
loadSession :: forall s v. HasManager s => Handler s v ()
loadSession = withTop' (toManager :: SnapletLens (Snaplet s) (ManagerFor s)) managerLoad


-- | Tells the session manager to persist any changes.
-- Should be called at the end of a request cycle.
commitSession :: forall s v. (HasManager s) => Handler s v ()
commitSession = withTop' (toManager :: SnapletLens (Snaplet s) (ManagerFor s)) managerCommit


-- | Obtain the local session for the current snaplet.
getSession :: forall s t. (HasManager s, AccessSession t, GlobalStateType t ~ ManagedState (ManagerFor s)) => Handler s t (StateType t)
getSession = do
    fs <- getFullSession
    return $ fs^.accessSession (error "Do not evaluate!" :: t)


modifyFullSession :: forall s v. HasManager s => (ManagedState (ManagerFor s) -> ManagedState (ManagerFor s)) -> Handler s v (ManagedState (ManagerFor s))
modifyFullSession f = withTop' (toManager :: SnapletLens (Snaplet s) (ManagerFor s)) $ managerModifySession f


-- | Set the local part of the session state to a new value
setSession :: forall s t. (HasManager s, AccessSession t, GlobalStateType t ~ ManagedState (ManagerFor s)) => StateType t -> Handler s t ()
setSession inner = void $ modifyFullSession (accessSession (error "Do not evaluate!" :: t) .~ inner)


-- | Modify the local session state with a function, returns the altered local state
--
-- @
--      setSession v = modifySession (const v)
--      getSession = modifySession id
-- @
modifySession :: forall s t. (HasManager s, AccessSession t, GlobalStateType t ~ ManagedState (ManagerFor s)) => (StateType t -> StateType t) -> Handler s t (ManagedState (ManagerFor s))
modifySession f = modifyFullSession (accessSession (error "Do not evaluate!" :: t) %~ f)
