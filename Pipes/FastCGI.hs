{-# LANGUAGE OverloadedStrings, NamedFieldPuns #-}

module Pipes.FastCGI (
  fromContentData, 
  toContentData,
  toContentDataUTF8,
  
  ServerSentEvent (..),
  serverSentEvent,
  streamEvents
  ) where

import Data.Maybe (fromMaybe)

import Pipes
import Pipes.Safe

import Network.FastCGI (MonadFastCGI, fGet, fPut,
                        setResponseStatus, setResponseHeader,
                        Header (HttpContentType))

import qualified Data.ByteString as BS

import qualified Data.Text as T
import qualified Data.Text.Encoding as T (encodeUtf8)

fromContentData :: MonadFastCGI m => Int -> Producer' BS.ByteString m ()
fromContentData size = loop where
  loop = do
    bs <- lift (fGet size)
    if BS.null bs
       then return ()
       else yield bs >> loop

toContentData :: MonadFastCGI m => Consumer' BS.ByteString m r
toContentData = for cat (lift . fPut)

toContentDataUTF8 :: MonadFastCGI m => Consumer' T.Text m r
toContentDataUTF8 = for cat (lift . fPut . T.encodeUtf8)

data ServerSentEvent =
  ServerSentEvent {
    eventName :: Maybe T.Text,
    eventId :: Maybe T.Text,
    eventData :: Maybe T.Text
    }

serverSentEvent :: ServerSentEvent
serverSentEvent = ServerSentEvent {
  eventName = Nothing,
  eventId = Nothing,
  eventData = Nothing }

streamEvents :: MonadFastCGI m => Consumer' ServerSentEvent m r
streamEvents = do
  lift $ setResponseStatus 200
  lift $ setResponseHeader HttpContentType "text/event-stream"
  serverSentEvents >-> toContentDataUTF8

serverSentEvents :: MonadFastCGI m => Pipe ServerSentEvent T.Text m r
serverSentEvents = for cat (yield . render)

render :: ServerSentEvent -> T.Text
render (ServerSentEvent { eventName, eventData, eventId } ) =
  T.concat [field "event" eventName,
            dataField eventData,
            field "id" eventId,
            "\n"]
  
field :: T.Text -> Maybe T.Text -> T.Text
field name (Just content) = T.concat [name, ": ", content, "\n"]
field _ Nothing = ""

dataField :: Maybe T.Text -> T.Text
dataField (Just x) = T.concat . map (\x -> T.concat ["data: ", x, "\n"]) $ T.lines x
dataField Nothing = ""
