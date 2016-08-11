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


class Manager manager where
    type ManagedState manager
    managerGetSession :: (MonadSnap m, MonadState manager m) => m (ManagedState manager)
    managerGetSession = managerModifySession id
    managerSetSession :: (MonadSnap m, MonadState manager m) => ManagedState manager -> m ()
    managerSetSession = managerModifySession . const >=> const (return ())
    managerCommit :: (MonadSnap m, MonadState manager m) => m ()
    managerLoad :: (MonadSnap m, MonadState manager m) => m ()

    managerModifySession :: (MonadSnap m, MonadState manager m) => (ManagedState manager -> ManagedState manager) -> m (ManagedState manager)
    managerModifySession f = managerGetSession >>= \sess -> managerSetSession (f sess) >> return sess


class Manager m => HasManager a m | a -> m where
    toManager :: SnapletLens (Snaplet a) m


type AccessSessionLens t a b = t -> Lens' a b


mkAccessSessionLens :: Lens' a b -> AccessSessionLens t a b
mkAccessSessionLens lens _ = lens


class AccessSession t where
    type GlobalStateType t
    type StateType t
    accessSession :: AccessSessionLens t (GlobalStateType t) (StateType t)


initSessionSnaplet :: Manager a => a -> SnapletInit b a
initSessionSnaplet man = makeSnaplet "session-manager" "manages typed sessions" Nothing $ return man


getFullSession :: forall s m v. (HasManager s m) => Handler s v (ManagedState m)
getFullSession = withTop' (toManager :: SnapletLens (Snaplet s) m) managerGetSession


loadSession :: forall s m v. (HasManager s m) => Handler s v ()
loadSession = withTop' (toManager :: SnapletLens (Snaplet s) m) managerLoad


commitSession :: forall s m v. (HasManager s m) => Handler s v ()
commitSession = withTop' (toManager :: SnapletLens (Snaplet s) m) managerCommit


getSession :: forall s t m. (HasManager s m, AccessSession t, GlobalStateType t ~ ManagedState m) => Handler s t (StateType t)
getSession = do
    fs <- getFullSession
    return $ fs^.accessSession (undefined :: t)


setFullSession :: forall s m v. (HasManager s m) => ManagedState m -> Handler s v ()
setFullSession sess = withTop' (toManager :: SnapletLens (Snaplet s) m) $ managerSetSession sess

modifyFullSession :: forall s m v. (HasManager s m) => (ManagedState m -> ManagedState m) -> Handler s v (ManagedState m)
modifyFullSession f = withTop' (toManager :: SnapletLens (Snaplet s) m) $ managerModifySession f

setSession :: forall s t m. (HasManager s m, AccessSession t, GlobalStateType t ~ ManagedState m) => StateType t -> Handler s t ()
setSession inner = modifyFullSession (accessSession (undefined :: t) .~ inner) >> return ()


modifySession :: forall s t m. (HasManager s m, AccessSession t, GlobalStateType t ~ ManagedState m) => (StateType t -> StateType t) -> Handler s t (ManagedState m)
modifySession f = modifyFullSession (accessSession (undefined :: t) %~ f)
