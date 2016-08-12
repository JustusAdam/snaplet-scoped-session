{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE ExplicitForAll      #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE RankNTypes          #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}
module Snap.Snaplet.Session.Scoped
    ( HasManager, TheManager, toManager, Manages
    , Manager, managerGetSession, managerSetSession, managerModifySession, managerCommit, managerLoad
    , AccessSession, LocalSession, accessSession, AccessSessionLens, mkAccessSessionLens
    , CanAccessSubsession
    , initSessionSnaplet
    , getSession, setSession, modifySession, loadSession, commitSession
    ) where


import           ClassyPrelude
import           Control.Lens
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
    type Manages manager
    managerGetSession :: Handler v manager (Manages manager)
    managerGetSession = managerModifySession id
    managerSetSession :: Manages manager -> Handler v manager ()
    managerSetSession = managerModifySession . const >=> const (return ())
    managerCommit :: Handler v manager ()
    managerCommit = return ()
    managerLoad :: Handler v manager ()
    managerLoad = return ()

    managerModifySession :: (Manages manager -> Manages manager) -> Handler v manager (Manages manager)
    managerModifySession f = managerGetSession >>= \sess -> managerSetSession (f sess) >> return sess

    {-# MINIMAL managerGetSession, managerSetSession | managerModifySession #-}


-- | Class providing access to a Snaplet managing session state
class Manager (TheManager a) => HasManager a where
    type TheManager a
    toManager :: SnapletLens (Snaplet a) (TheManager a)


type CanAccessSubsession a b = (HasManager a, AccessSession (Manages (TheManager a)) b)


-- | Type magic
type AccessSessionLens t a b = t -> Lens' a b


-- | You should use this function to create a 'AccessSessionLens'. It ignores
-- the 't' argument. The 't' ergument is only used to line up types.
mkAccessSessionLens :: Lens' a b -> AccessSessionLens t a b
mkAccessSessionLens = const


-- | A reference to a LocalSession session state from the GlobalSession session state
-- You have to implement this class for your subsnaplet to get access to a part of the GlobalSession session state.
class AccessSession base t where
    type LocalSession t
    accessSession :: AccessSessionLens t base (LocalSession t)


-- | Initialize a session managing Snaplet from a manager. For an example manager see
-- 'Snap.Snaplet.Session.Scoped.InMemory'
initSessionSnaplet :: Manager a => a -> SnapletInit b a
initSessionSnaplet man = makeSnaplet "session-manager" "manages typed sessions" Nothing $ return man


getFullSession :: forall s v. HasManager s => Handler s v (Manages (TheManager s))
getFullSession = withTop' (toManager :: SnapletLens (Snaplet s) (TheManager s)) managerGetSession


-- | Tells the session state manager to load the session.
-- Users should not have to call this function
loadSession :: forall s v. HasManager s => Handler s v ()
loadSession = withTop' (toManager :: SnapletLens (Snaplet s) (TheManager s)) managerLoad


-- | Tells the session manager to persist any changes.
-- Should be called at the end of a request cycle.
commitSession :: forall s v. HasManager s => Handler s v ()
commitSession = withTop' (toManager :: SnapletLens (Snaplet s) (TheManager s)) managerCommit


-- | Obtain the LocalSession session for the current snaplet.
getSession :: forall s t. (HasManager s, AccessSession (Manages (TheManager s)) t) => Handler s t (LocalSession t)
getSession = do
    fs <- getFullSession
    return $ fs^.accessSession (error "Do not evaluate!" :: t)


modifyFullSession :: forall s v. HasManager s => (Manages (TheManager s) -> Manages (TheManager s)) -> Handler s v (Manages (TheManager s))
modifyFullSession f = withTop' (toManager :: SnapletLens (Snaplet s) (TheManager s)) $ managerModifySession f


-- | Set the LocalSession part of the session state to a new value
setSession :: forall s t. (HasManager s, AccessSession (Manages (TheManager s)) t) => LocalSession t -> Handler s t ()
setSession inner = void $ modifyFullSession (accessSession (error "Do not evaluate!" :: t) .~ inner)


-- | Modify the LocalSession session state with a function, returns the altered LocalSession state
--
-- @
--      setSession v = modifySession (const v)
--      getSession = modifySession id
-- @
modifySession :: forall s t. (HasManager s, AccessSession (Manages (TheManager s)) t) => (LocalSession t -> LocalSession t) -> Handler s t (Manages (TheManager s))
modifySession f = modifyFullSession (accessSession (error "Do not evaluate!" :: t) %~ f)
