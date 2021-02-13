{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ScopedTypeVariables #-}

{-|
Copyright   : (c) 2018-2019, Commonwealth Scientific and Industrial Research Organisation
License     : BSD3
Maintainer  : dave.laing.80@gmail.com, jack.kelly@data61.csiro.au
Stability   : experimental
Portability : non-portable
-}

module Main (main) where

import           Control.Lens (_1, _2, _3, view)
import           Control.Monad (void)
import           Control.Monad.Fix (MonadFix)
import           Control.Monad.IO.Class (MonadIO(..))
import           Data.ByteString (ByteString)
import qualified Data.ByteString.Char8 as BC
import           Data.Char (isSpace)
import           Data.Function ((&))
import           Data.Functor ((<&>))
import           Data.Map (Map)
import qualified Data.Map as Map
import qualified Network.Socket as NS
import           Reflex
import           Reflex.Backend.Socket
import           Reflex.Host.Headless
import           System.IO.Error

type ServerCxt t m =
  ( Reflex t
  , MonadFix m
  , MonadSample t m
  , PerformEvent t m
  , PostBuild t m
  , TriggerEvent t m
  , MonadIO m
  , MonadIO (Performable m)
  )

perConnection
  :: ServerCxt t m
  => Dynamic t NS.Socket
  -> Event t ByteString
  -> m (Event t ByteString, Event t (), Event t ())
perConnection dSocket eTx = mdo
  s <- sample . current $ dSocket
  so <- socket $ SocketConfig s 4096 eTx eQuit

  let
    eRx = _sReceive so
    eRxLine = fst . BC.spanEnd isSpace <$> eRx
    eQuit = void $ ffilter (== "quit") eRxLine
    eShutdown = void $ ffilter (== "shutdown") eRxLine
    eOut = ffilter (`notElem` ["quit", "shutdown"]) eRxLine

  pure (eOut, eShutdown, _sClose so)

go :: forall t m. MonadHeadlessApp t m => m (Event t ())
go = mdo
  (eListenError, eAccept) <- fanEither <$> accept
    (AcceptConfig (Just "127.0.0.1") (Just "9000") 1 [(NS.ReuseAddr, 1)] eQuit)

  eNewClient <- switchHold never $ fmap fst . _aAcceptSocket <$> eAccept
  eListenClose <- switchHold never $ _aClose <$> eAccept
  eAcceptError <- switchHold never $ _aError <$> eAccept

  performEvent_ $ eListenError
    <&> liftIO . putStrLn . ("Listen error: " <>) . show
  performEvent_ $ eAcceptError
    <&> liftIO . putStrLn . ("Error accepting: " <>) . show . ioeGetErrorType

  eAdds :: Event t (Integer, NS.Socket) <- numberOccurrences eNewClient

  -- Maintain a map of all open sockets, indexed by their occurrence number.
  dSocketMap <- foldDyn ($) Map.empty . mergeWith (.) $
    [ uncurry Map.insert <$> eAdds
    , flip (foldr Map.delete) <$> eRemoves
    ]

  dmeSocketEvents
    :: Dynamic t (Map Integer (Event t ByteString, Event t (), Event t ()))
    <- list dSocketMap $ \dSocket -> perConnection dSocket eChats

  let
    eChats :: Event t ByteString
    eChats = dmeSocketEvents
      -- Pick out the "data received" events, and go from "dynamic map of
      -- events" to "dynamic of (event of map)"
      & fmap (mergeMap . fmap (view _1))

      -- listen to the latest Event
      & switchDyn

      -- Collect all the received data into one big ByteString
      & fmap (mconcat . Map.elems)

    eShutdown = dmeSocketEvents
      & fmap (mergeMap . fmap (view _2))
      & switchDyn
      & void

    eRemoves = dmeSocketEvents
      & fmap (mergeMap . fmap (view _3))
      & switchDyn
      & fmap Map.keys

    eQuitMap = void . ffilter id . updated $ Map.null <$> dSocketMap
    eQuit = leftmost
      [ void eListenError -- Couldn't open listen socket
      , eQuitMap -- Nobody is connected
      , eListenClose -- Our listen socket was closed
      , eShutdown -- Client asked us to shutdown everything
      ]
  pure eQuit

main :: IO ()
main = runHeadlessApp go
