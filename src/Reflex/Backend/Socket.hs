{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}

{-|
Copyright   : (c) 2018-2019, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com
Stability   : experimental
Portability : non-portable
-}

module Reflex.Backend.Socket
  ( socket

    -- * Socket Configuration
  , SocketConfig(..)

    -- * Socket Output Events
  , Socket(..)

    -- * Convenience Re-Exports
  , connect
  , module Reflex.Backend.Socket.Accept

  -- * Lenses
  -- ** SocketConfig
  , scInitSocket
  , scMaxRx
  , scSend
  , scClose

  -- ** Socket
  , sReceive
  , sOpen
  , sClosed
  , sError
  ) where

import           Control.Concurrent (forkIO)
import qualified Control.Concurrent.STM as STM
import           Control.Exception (IOException, catch, try)
import           Control.Lens.TH (makeLenses)
import           Control.Monad.IO.Class (MonadIO(..))
import           Control.Monad.STM (atomically, orElse)
import           Data.ByteString (ByteString)
import           Data.Foldable (for_)
import           Data.Functor (($>), (<&>), void)
import qualified Network.Socket as NS
import           Network.Socket.ByteString (sendAll, recv)
import           Reflex
import           Reflex.Backend.Socket.Accept
import           Reflex.Backend.Socket.Connect (connect)

data SocketConfig t = SocketConfig
  { _scInitSocket :: NS.Socket
  , _scMaxRx      :: Int
  , _scSend       :: Event t ByteString
  , _scClose      :: Event t ()
  }

$(makeLenses ''SocketConfig)

-- | Events produced by an active socket.
data Socket t = Socket
  { _sReceive :: Event t ByteString
    -- ^ Data has arrived.
  , _sOpen    :: Event t ()
    -- ^ The socket is open, and its receive/send loops are running.
  , _sClosed  :: Event t ()
    -- ^ The socket has closed. This will fire exactly once when the
    -- socket closes for any reason, including if your '_scClose'
    -- event fires, the other end disconnects, or if the socket closes
    -- in response to a caught exception.
  , _sError   :: Event t IOException
    -- ^ An exception occurred. Treat the socket as closed after you
    -- see this. If the socket was open, you will see the '_sClosed'
    -- event fire as well, but not necessarily in the same frame.
  }

$(makeLenses ''Socket)

data TxStepResult
  = Send NS.Socket ByteString
  | Close NS.Socket

-- | Wire a socket into the FRP network. You will likely use this to
-- attach events to a socket that you just connected (from 'connect'),
-- or a socket that you just accepted (from the '_aAcceptSocket' event
-- you got when you called 'accept').
socket
  :: forall t m.
     ( Reflex t
     , PerformEvent t m
     , PostBuild t m
     , TriggerEvent t m
     , MonadIO (Performable m)
     , MonadIO m
     )
  => SocketConfig t
  -> m (Socket t)
socket (SocketConfig initSock maxRx eTx eClose) = mdo
  (eRx, onRx) <- newTriggerEvent
  (eOpen, onOpen) <- newTriggerEvent
  (eError, onError) <- newTriggerEvent
  (eClosed, onClosed) <- newTriggerEvent

  payloadQueue <- liftIO STM.newTQueueIO
  closeQueue <- liftIO STM.newEmptyTMVarIO
  isOpen <- liftIO $ STM.newTMVarIO initSock

  ePostBuild <- getPostBuild

  let
    start = liftIO $ do
      void $ forkIO txLoop
      void $ forkIO rxLoop
      onOpen ()

      where
        txLoop =
          let
            stmClose sock = STM.tryTakeTMVar closeQueue $> Close sock
            stmTx sock = STM.readTQueue payloadQueue >>= pure . Send sock

            loop =
              atomically
                (STM.readTMVar isOpen >>= (orElse <$> stmClose <*> stmTx))
              >>= \case
              Close sock -> do
                void . atomically $ STM.takeTMVar isOpen
                NS.close sock `catch` onError
                onClosed ()
              Send sock bs -> do
                try (sendAll sock bs) >>= \case
                  Left exc -> onError exc *> shutdown
                  Right () -> pure ()
                loop
          in loop

        rxLoop =
          let
            loop = do
              mSock <- atomically $ STM.tryReadTMVar isOpen
              for_ mSock $ \sock -> try (recv sock maxRx) >>= \case
                Left exc -> onError exc *> shutdown
                Right bs -> onRx bs *> loop
          in loop

        shutdown = void . atomically $ STM.tryPutTMVar closeQueue ()

  performEvent_ $ ePostBuild $> start

  performEvent_ $ eTx <&> \bs -> liftIO . atomically $
    STM.tryReadTMVar isOpen >>= \case
      Nothing -> pure ()
      Just{} -> STM.writeTQueue payloadQueue bs

  performEvent_ $ eClose $> liftIO (atomically $ STM.putTMVar closeQueue ())

  pure $ Socket eRx eOpen eClosed eError
