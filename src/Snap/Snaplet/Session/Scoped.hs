{-|
Module      : $Header$
Description : Abstract Manager class and Session Snaplet
Copyright   : (c) Justus Adam, 2016
License     : BSD3
Maintainer  : dev@justus.science
Stability   : experimental
Portability : POSIX
-}
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
-- 'managerLoad' is not required it is useful for efficiency reasons.
--
-- The state of the manager is mutable within one request but reset for new requests.
-- Therefore managers should use mutable or persistent data structures like 'IORef' internally.
class Manager manager where
    -- | Type of the session state inside the manager. In most cases this will
    -- either be any type or a type constrained by some serialisation typeclass like
    --
    -- @
    --  instance Serialize a => Manager (WritesToFileManager a) where
    --      type Manages (WritesToFileManager a) = a
    -- @
    type Manages manager
    -- | Obtain the full session state for the current client.
    managerGetSession :: Handler v manager (Manages manager)
    managerGetSession = managerModifySession id
    -- | Replace the state of the current sessio with the provided one
    managerSetSession :: Manages manager -> Handler v manager ()
    managerSetSession = managerModifySession . const >=> const (return ())

    -- | Indicate to the manager that cached mutations on state should be persisted.
    --
    -- This method is entirely optional, however it is recommended
    -- for managers where mutating the persistent state is expensive to
    -- cache mutations in the manager data structure and persist them once commit
    -- is called.
    managerCommit :: Handler v manager ()
    managerCommit = return ()

    -- | Again optional. Users can use this method to indicate to the manager that
    -- the session should be established (cookie read for instance) and the state
    -- cached into the manager data structure.
    managerLoad :: Handler v manager ()
    managerLoad = return ()

    -- | Applies a function to the (cached) session state
    managerModifySession :: (Manages manager -> Manages manager) -> Handler v manager (Manages manager)
    managerModifySession f = managerGetSession >>= \sess -> managerSetSession (f sess) >> return sess

    {-# MINIMAL managerGetSession, managerSetSession | managerModifySession #-}


-- | Class providing access to a Snaplet managing session state
class Manager (TheManager a) => HasManager a where
    -- | Type of the manager, since this is defined in terms of the 'Manager' typeclass
    type TheManager a
    -- | Like 'HasHeist', a lens to get to the manager snaplet.
    toManager :: SnapletLens (Snaplet a) (TheManager a)


-- | This is provided for convenience. Snaplets which use part of the state can
-- use this shorthand in the type signature.
--
--      * 'base' is the (internal) type of your root snaplet
--      * 'snaplet' is the (internal) type of the snaplet that wants to access the session
--      * 't' is the type the (local) session data
--
-- If, for instance, our Snaplet was called 'Files' and it needed a HashSet as state
-- and the global Snaplet type was unknown you could write the following:
--
-- @
--  initFiles :: (CanAccessSubsession b Files HashSet) => SnapletInit b Files
-- @
type CanAccessSubsession base snaplet t = (HasManager base, AccessSession (Manages (TheManager base)) snaplet, LocalSession snaplet ~ t)


-- | This is just type hacking to line up lenses. Use 'mkAccessSessionLens'
-- to turn a regular lens into this type.
newtype AccessSessionLens t a b = ASLens { getASLens :: t -> Lens' a b }


-- | Use this function to create an 'AccessSessionLens'. 't' is only used to line up types.
mkAccessSessionLens :: Lens' a b -> AccessSessionLens t a b
mkAccessSessionLens = ASLens . const


-- | A reference to a LocalSession session state from the global session state ('base').
-- You have to implement this class for your subsnaplet ('t') to get access to a part of the global session state.
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
    return $ fs^.getASLens accessSession (error "Do not evaluate!" :: t)


modifyFullSession :: forall s v. HasManager s => (Manages (TheManager s) -> Manages (TheManager s)) -> Handler s v (Manages (TheManager s))
modifyFullSession f = withTop' (toManager :: SnapletLens (Snaplet s) (TheManager s)) $ managerModifySession f


-- | Set the LocalSession part of the session state to a new value
setSession :: forall s t. (HasManager s, AccessSession (Manages (TheManager s)) t) => LocalSession t -> Handler s t ()
setSession inner = void $ modifyFullSession (getASLens accessSession (error "Do not evaluate!" :: t) .~ inner)


-- | Modify the LocalSession session state with a function, returns the altered LocalSession state
--
-- @
--      setSession v = modifySession (const v)
--      getSession = modifySession id
-- @
modifySession :: forall s t. (HasManager s, AccessSession (Manages (TheManager s)) t) => (LocalSession t -> LocalSession t) -> Handler s t (Manages (TheManager s))
modifySession f = modifyFullSession (getASLens accessSession (error "Do not evaluate!" :: t) %~ f)
