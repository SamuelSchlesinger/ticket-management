{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
module Web.Ticket.Api where

import Data.Ticket
import Servant.API

type TicketApi =
       "command" :> ReqBody '[JSON] [Command] :> PostNoContent
  :<|> "query" :> ReqBody '[JSON] Query :> Post '[JSON] [TicketDetails]
  :<|> "init" :> PostNoContent
  :<|> "validate" :> GetNoContent
  :<|> "graphviz" :> Capture "relationship-type" RelationshipType :> ReqBody '[JSON] Query :> Get '[PlainText, JSON] String
  :<|> Raw
