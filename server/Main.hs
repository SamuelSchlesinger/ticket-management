{-# LANGUAGE LambdaCase #-}
module Main where

import qualified Web.Ticket.Server as Server
import System.Environment (lookupEnv)
import System.Exit (exitFailure)

main :: IO ()
main = lookupEnv "TICKET_SYSTEM" >>= \case
  Just x -> Server.runServer x 3001
  Nothing -> putStrLn "Trying to start server without TICKET_SYSTEM environment variable set" >> exitFailure
