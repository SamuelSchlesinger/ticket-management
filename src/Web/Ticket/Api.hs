{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Web.Ticket.Api where

import Data.Ticket
import Servant.API
import Network.HTTP.Media.MediaType ((//))

data AllTypes

instance Accept AllTypes where
  contentType _ = "*" // "*"

instance MimeRender AllTypes () where
  mimeRender _ _ = ""

type TicketApi =
       "command" :> ReqBody '[JSON] [Command] :> PostNoContent
  :<|> "query" :> ReqBody '[JSON] Query :> Post '[JSON] [TicketDetails]
  :<|> "init" :> PostNoContent
  :<|> "validate" :> GetNoContent
  :<|> "graphviz" :> Capture "relationship-type" RelationshipType :> ReqBody '[JSON] Query :> Get '[PlainText, JSON] String
  :<|> UVerb 'GET '[AllTypes] '[WithStatus 302 (Headers '[Header "Location" String] ())]
  :<|> Raw
