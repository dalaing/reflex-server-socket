{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE LambdaCase          #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies        #-}

{-|
Copyright   : (c) 2018-2019, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com, jack.kelly@data61.csiro.au
Stability   : experimental
Portability : non-portable
-}

module Main (main) where

import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Char8 as BC
import           Data.Functor ((<&>), void)
import           Data.Maybe (isNothing)
import qualified Network.Socket as NS
import           Reflex
import           Reflex.Backend.Socket
import           Reflex.Host.Headless
import           Reflex.Network (networkHold)
import           System.Environment (getArgs)
import           Witherable (catMaybes)

-- | Connect to a remote host, and quit as soon as something happens.
connect1 :: IO ()
connect1 = runHeadlessApp $ do
  eQuit <- connect (Just "127.0.0.1") "9000"
  let (eError, eConnect) = fanEither eQuit

  performEvent_ $ (liftIO . putStrLn $ "Connected") <$ eConnect
  performEvent_ $ liftIO . print <$> eError

  pure (void eQuit)

-- | Listen for connections, and log them as they come in. Does
-- nothing with arriving connections, so will leak FDs.
accept1 :: IO ()
accept1 = runHeadlessApp $ do
  (eListenError, eAccept) <- fanEither <$> accept
    (AcceptConfig (Just "127.0.0.1") (Just "9000") 1 [(NS.ReuseAddr, 1)] never)

  eNewClient <- switchHold never $ _aAcceptSocket <$> eAccept
  eListenClosed <- switchHold never $ _aClose <$> eAccept
  eAcceptError <- switchHold never $ _aError <$> eAccept

  performEvent_ $
    (liftIO . putStrLn $ "Error starting listen socket") <$ eListenError
  performEvent_ $ (liftIO . putStrLn $ "Listen closed") <$ eListenClosed
  performEvent_ $ (liftIO . putStrLn $ "Connected") <$ eNewClient
  performEvent_ $ liftIO . print <$> eAcceptError

  pure never

-- | Connect to a remote host. When the connection succeeds, put the
-- @'Socket' t@ into @dSocket@, send a message, then close.
connect2 :: IO ()
connect2 = runHeadlessApp $ mdo
  (eConnError, eConnect) <- fanEither <$> connect (Just "127.0.0.1") "9000"

  performEvent_ $ (liftIO . putStrLn $ "Connected") <$ eConnect
  performEvent_ $ liftIO . print <$> eConnError

  dSocket :: Dynamic t (Maybe (Socket t)) <- networkHold (pure Nothing) $ leftmost
    [ eConnect <&> \s -> Just <$> socket (SocketConfig s 2048 eTx eOpen)
    , pure Nothing <$ eClosed
    ]

  eOpen <- switchHold never . fmap _sOpen . catMaybes $ updated dSocket
  let eTx = "Hi" <$ eOpen

  eClosed <- switchHold never . fmap _sClose . catMaybes $ updated dSocket
  eRx <- switchHold never . fmap _sReceive . catMaybes $ updated dSocket
  eSockError <- switchHold never . fmap _sError . catMaybes $ updated dSocket

  performEvent_ $ liftIO . BC.putStrLn <$> eRx
  performEvent_ $ liftIO . print <$> eSockError
  performEvent_ $ liftIO (putStrLn "Closed") <$ eClosed

  let eQuit = leftmost
        [ void eConnError
        , void . ffilter isNothing $ updated dSocket
        ]

  pure eQuit

main :: IO ()
main = getArgs >>= \case
  ["accept1"] -> accept1
  ["connect1"] -> connect1
  ["connect2"] -> connect2
  _ -> pure ()
