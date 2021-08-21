{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE BlockArguments #-}
module Web.Ticket.Server (runServer) where

import Servant
import Data.Ticket
import TicketManager
import Control.Monad.IO.Class (liftIO)
import Web.Ticket.Api (TicketApi)
import Network.Wai.Handler.Warp (run, Port)

runServer :: FilePath -> Port -> IO ()
runServer filepath port = run port $ application filepath

application :: FilePath -> Application
application = serve (Proxy @TicketApi) . server

server :: FilePath -> Server TicketApi
server filepath = postCommands filepath :<|> getQuery filepath :<|> postInit filepath :<|> getValidate filepath :<|> getGraphViz filepath

postCommands :: FilePath -> [Command] -> Handler NoContent
postCommands filepath cs = NoContent <$ (liftIO $ runCommands filepath cs)

getQuery :: FilePath -> Query -> Handler [TicketDetails]
getQuery filepath q = liftIO $ runQuery filepath q

getValidate :: FilePath -> Handler NoContent
getValidate filepath = liftIO (runValidate filepath) >>= \case
  True -> pure NoContent
  False -> throwError err500 { errBody = "The ticket data is corrupted" }

postInit :: FilePath -> Handler NoContent
postInit filepath = liftIO (runInit filepath) >>= \case
  True -> pure NoContent
  False -> throwError err409 { errBody = "Trying to initialize a pre-existing ticket system" }

getGraphViz :: FilePath -> RelationshipType -> Query -> Handler String
getGraphViz filepath relType q = liftIO (runGraphViz filepath relType q)
