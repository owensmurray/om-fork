{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE ImplicitParams #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}

{- | Description: Thread utilities. -}
module OM.Fork (
  -- * Actor Communication.
  Actor(..),
  Responder,
  Responded,
  respond,
  call,
  cast,

  -- * Forking Background Processes.
  logUnexpectedTermination,
  ProcessName(..),
  Race,
  runRace,
  race,
  wait,
) where


import Control.Concurrent (Chan, myThreadId, newEmptyMVar, putMVar,
  takeMVar, writeChan)
import Control.Monad (void)
import Control.Monad.Catch (MonadThrow(throwM), MonadCatch, SomeException,
  try)
import Control.Monad.IO.Class (MonadIO, liftIO)
import Control.Monad.Logger.CallStack (MonadLogger, logWarn)
import Data.Aeson (ToJSON, toJSON)
import Data.String (IsString)
import Data.Text (Text)
import OM.Show (showt)
import UnliftIO (MonadUnliftIO, askRunInIO, throwString)
import qualified Ki


{- | How to respond to a asynchronous message. -}
newtype Responder a = Responder {
    unResponder :: a -> IO ()
  }
instance ToJSON (Responder a) where
  toJSON _ = toJSON ("<Responder>" :: Text)
instance Show (Responder a) where
  show _ = "Responder"


{- | The class of types that can act as the handle for an asynchronous actor. -}
class Actor a where
  {- | The type of messages associated with the actor. -}
  type Msg a
  {- | The channel through which messages can be sent to the actor. -}
  actorChan :: a -> Msg a -> IO ()
instance Actor (Chan m) where
  type Msg (Chan m) = m
  actorChan = writeChan


{- | Respond to an asynchronous message. -}
respond :: (MonadIO m) => Responder a -> a -> m Responded
respond responder val = do
  liftIO (unResponder responder val)
  return Responded


{- | Send a message to an actor, and wait for a response. -}
call :: (Actor actor, MonadIO m) => actor -> (Responder a -> Msg actor) -> m a
call actor mkMessage = liftIO $ do
  mVar <- newEmptyMVar
  actorChan actor (mkMessage (Responder (putMVar mVar)))
  takeMVar mVar


{- | Send a message to an actor, but do not wait for a response. -}
cast :: (Actor actor, MonadIO m) => actor -> Msg actor -> m ()
cast actor = liftIO . actorChan actor


{- |
  Proof that 'respond' was called. Clients can use this type in their
  type signatures when they require that 'respond' be called at least
  once, because calling 'respond' is the only way to generate values of
  this type.
-}
data Responded = Responded


{- | Log (at WARN) when the action terminates for any reason. -}
logUnexpectedTermination :: (MonadLogger m, MonadCatch m)
  => ProcessName
  -> m a
  -> m a
logUnexpectedTermination (ProcessName name) action =
  try action >>= \case
    Left err -> do
      logWarn
        $ "Action " <> name <> " finished with an error: " <> showt err
      throwM (err :: SomeException)
    Right v -> do
      logWarn $ "Action " <> name <> " finished normally."
      return v


{- |
  Run a thread scope. When the scope terminates, all threads (created with
  'fork_') within the scope are terminated.
-}
runRace :: (MonadUnliftIO m) => (Race => m a) -> m a
runRace action = do
  runInIO <- askRunInIO
  liftIO . Ki.scoped $ \scope ->
    runInIO (let ?scope = scope in action)


{- |
  This constraint indicates that we are in the context of a thread race. If any
  threads in the race terminate, then all threads in the race terminate.
  Threads are "in the race" if they were forked using 'race'.
-}
type Race = (?scope :: Ki.Scope)


{- |
  Fork a new thread within the context of a race. This thread will be
  terminated when any other racing thread terminates, or else if this
  thread terminates first, it will cause all other racing threads to
  be terminated.
-}
race
  :: ( MonadCatch m
     , MonadLogger m
     , MonadUnliftIO m
     , Race
     )
  => ProcessName
  -> m a
  -> m ()
race name action = do
  runInIO <- askRunInIO
  liftIO
    . Ki.fork_ ?scope
    $ do
      runInIO . logUnexpectedTermination name $ void action
      tid <- myThreadId
      throwString $ "Thread Finished: " <> show tid


{- | The name of a process. -}
newtype ProcessName = ProcessName
  { unProcessName :: Text
  }
  deriving newtype (IsString, Semigroup, Monoid)


{- | Wait for all racing threads to terminate. -}
wait :: (MonadIO m, Race) => m ()
wait = liftIO $ Ki.wait ?scope


