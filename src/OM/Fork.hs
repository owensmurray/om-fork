{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

{- | Provides the `ForkM` typeclass, and other related utilities. -}
module OM.Fork (
  Actor(..),
  Responder,
  Responded,
  respond,
  call,
  cast,
  logUnexpectedTermination,
) where


import Control.Concurrent (newEmptyMVar, putMVar, takeMVar, Chan,
  writeChan)
import Control.Exception.Safe (SomeException, tryAsync, throw, MonadCatch)
import Control.Monad.IO.Class (liftIO, MonadIO)
import Control.Monad.Logger (logWarn, MonadLogger)
import Data.Text (Text)
import OM.Show (showt)


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


{- | Log (at WARN) when an terminates for any reason. -}
logUnexpectedTermination :: (MonadLogger m, MonadCatch m)
  => Text
  -> m a
  -> m a
logUnexpectedTermination name action =
  tryAsync action >>= \case
    Left err -> do
      $(logWarn)
        $ "Action " <> name <> " finished with an error: " <> showt err
      throw (err :: SomeException)
    Right v -> do
      $(logWarn) $ "Action " <> name <> " finished normally."
      return v


