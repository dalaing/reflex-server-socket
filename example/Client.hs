{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

{-|
Copyright   : (c) 2018-2019, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com, jack.kelly@data61.csiro.au
Stability   : experimental
Portability : non-portable
-}

module Main (main) where

import           Control.Concurrent (forkIO)
import           Control.Monad (void, forever)
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.IO.Class (MonadIO(..))
import qualified Data.ByteString.Char8 as BC
import qualified Network.Socket as NS
import           Reflex
import           Reflex.Backend.Socket
import           Reflex.Host.Headless
import           Reflex.Workflow

type ClientCxt t m =
  ( Reflex t
  , MonadFix m
  , PerformEvent t m
  , PostBuild t m
  , TriggerEvent t m
  , MonadIO m
  , MonadIO (Performable m)
  )

unconnected :: ClientCxt t m => Workflow t m (Event t ())
unconnected = Workflow $ do
  (eError, eConnected) <- fanEither <$> connect (Just "127.0.0.1") "9000"
  eReportedError <-
    performEvent $
    (\e -> liftIO $ putStrLn "Error:\n" *> print e) <$>
    eError
  pure (() <$ eReportedError, connected <$> eConnected)

connected :: ClientCxt t m => NS.Socket -> Workflow t m (Event t ())
connected s = Workflow $ mdo
  (eLine, onLine) <- newTriggerEvent
  void . liftIO . forkIO . forever $ onLine . BC.pack =<< getLine

  so <- socket $ SocketConfig s 2048 eLine eQuit
  performEvent_ $ liftIO . BC.putStrLn <$> _sReceive so

  let eQuit = _sClose so
  pure (eQuit, unconnected <$ eQuit)

main :: IO ()
main = runHeadlessApp $ do
  deQuit <- workflow unconnected
  pure (switchDyn deQuit)
