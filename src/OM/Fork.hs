{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}

{- | Provides the `ForkM` typeclass, and other related utilities. -}
module OM.Fork (
  forkC,
  Actor(..),
  Responder,
  Responded,
  respond,
  call,
  cast,
) where

import Control.Concurrent (forkIO, newEmptyMVar, putMVar, takeMVar,
   Chan, writeChan)
import Control.Exception.Safe (tryAny)
import Control.Monad (void)
import Control.Monad.IO.Class (liftIO, MonadIO)
import System.Exit (ExitCode(ExitFailure))
import System.IO (hPutStrLn, stderr, hFlush, stdout)
import System.Posix.Process (exitImmediately)

{- |
  Forks a critical thread. \"Critical\" in this case means that if the
  thread crashes for whatever reason, then the program cannot continue
  correctly, so we should crash the program instead of running in some
  kind of zombie broken state.
-}
forkC :: (MonadIO m)
  => String {- ^ The name of the critical thread, used for logging. -}
  -> IO () {- ^ The IO to execute. -}
  -> m ()
forkC name action =
  void . liftIO . forkIO $
    tryAny action >>= \case
      Left err -> do
        let msg =
              "Exception caught in critical thread " ++ show name
              ++ ". We are crashing the entire program because we can't "
              ++ "continue without this thread. The error was: "
              ++ show err
        {- write the message to every place we can think of. -}
        liftIO (putStrLn msg)
        liftIO (hPutStrLn stderr msg)
        liftIO (hFlush stdout)
        liftIO (hFlush stderr)
        liftIO (exitImmediately (ExitFailure 1))
      Right v -> return v


{- | How to respond to a asynchronous message. -}
newtype Responder a = Responder {
    unResponder :: a -> IO ()
  }
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


