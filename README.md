# Scoped sessions

This snaplet provides a convenient way to handle typed sessions in your snap application.

It uses lenses to separate your session state into smaller parts which can then be individually managed by nested snaplets.
The session state is therefore separated and subsnaplets have no access to the full global session state.

## Session management

Session management inside your application is done by a session `Snaplet`. The Snaplet is created using `initSessionSnaplet` and it expects a `Manager` as an argument.
Usually you won't have to create this manager yourself, simply find a library function which provides you with one that suits your needs and drop it in there.
As an example there is a in memory which you can create with the function `initMemoryManager` from `Snap.Snaplet.Session.Scoped.InMemory`. You can find more information on this particular manager in the section [The InMemory Manager](#the-inmemory-manager)

### The Manager typeclass

`Manager`s are responsible for persisting the session state in whichever way he sees fit. Depending on your choice of manager therefore your session can be kept in memory or a database or persistent in files on the server.
Furthermore the manager handles the connection to the client, which in most cases will be a session cookie.

Managers are parameterised by the state they can manage. It uses the TypeInType extension to define the internal state.


For some Managers this may be any type

```Haskell
instance Manager (MyManager a) where
    type Manages (MyManager a) = a -- any state
```

or a constrained type

```Haskell
instance Serialize a => Manager (MySerializingManager a) where
    type Manages (MySerializingManager a) = a
```

or even a concrete type

```Haskell
instance Manager MyCountingManager where
    type Manages MyCountingManager = Int
```

#### The InMemory Manager

`Snap.Snaplet.Session.Scoped.InMemory` defines a `Manager` which stores arbitrary server side session state in the form of in-memory Haskell data. Session tracking is done using a configurable cookie and sessions are automatically expired to free memory by a concurrent worker which cleans the session state.

An example of the configuration can be found in `resources/devel.cfg`. All values are optional.


## Wiring the Application

In order to give your snaplets access to the session manager you have to specify how the manager snaplet can be reached from inside your application.
This is done by implementing a typeclass called `HasManager`.

```Haskell
data MyApp = MyApp { _sessionSnaplet :: Snaplet MyManager, ... }

makeLenses ''MyApp

instance HasManager MyApp where
    type TheManager MyApp = MyManager
    toManager = subSnaplet sessionSnaplet
```

Then, for each subsnaplet which is to have access to a part of the state you have to implement `AccessSession` to define the substate a Snaplet has access to.

```Haskell
data Session = Session
    { _snapletA :: SnapletASession
    , _snapletB :: SnapletBSession
    }

makeLenses SessionState

instance AccessSession Session SnapletA where
    type LocalSession SnapletA = SnapletASession
    accessSession = mkAccessSessionLens snapletA

instance AccessSession Session SnapletB where
    type LocalSession SnapletB = SnapletBSession
    accessSession = mkAccessSessionLens snapletB
```

## Feedback

Feedback for the implementation and the design decisions of this library is very welcome in the form of [issues](issues) or even better, a [pull request](compare).

Feedback for the documentation is very welcome as well in the form of an [issue](issues).
