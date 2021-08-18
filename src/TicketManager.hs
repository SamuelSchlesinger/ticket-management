{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module TicketManager (main) where

import Data.Ticket
import Options.Applicative
import Imports
import qualified Data.ByteString as BS

main :: IO ()
main = customExecParser ps parser >>= program where
  ps :: ParserPrefs
  ps = prefs . mconcat $
    [ disambiguate
    , showHelpOnError
    ]
  program :: TicketStatement -> IO ()
  program ticketStatement = lookupEnv "TICKET_SYSTEM" >>= \case
    Nothing -> do
      fail "No TICKET_SYSTEM environment variable set"
    Just filepath -> do
      case ticketStatement of
        TicketAction cmd -> appendTicketActions filepath [cmd]
        TicketQuery q -> case q of
          GetTicket ticketID -> withTicketSystem filepath \ticketSystem -> do
            getTicket ticketID (ticketModel ticketSystem) >>= print
        TicketInitialize -> do
          doesFileExist filepath >>= \case
            True -> fail "Trying to initialize a pre-existing ticket system"
            False -> BS.writeFile filepath (encode emptyTicketSystem)
  parser :: ParserInfo TicketStatement
  parser = flip info mods . hsubparser . mconcat $
    [ command "create" (info create (progDesc "Creates a new ticket"))
    , command "edit-name" (info editName (progDesc "Changes the name of an existing ticket"))
    , command "edit-status" (info editStatus (progDesc "Changes the status of an existing ticket"))
    , command "edit-description" (info editDescription (progDesc "Changes the description of an existing ticket"))
    , command "relate" (info relate (progDesc "Relates two tickets to each other"))
    , command "unrelate" (info unrelate (progDesc "Removes the relationship between two tickets"))
    , command "get" (info get (progDesc "Gets the details of the ticket"))
    , command "init" (info init (progDesc "Initializes an empty ticket system"))
    ]
  mods = header "Ticket Manager!" <> footer "Copyright 2021 (c) Samuel Schlesinger" <> progDesc "Allows the user to manage work tickets."
  ticketIDArgument = strArgument (metavar "TICKET_ID")
  nameOption = strOption (long "name" <> short 'n')
  statusOption = option auto (long "status" <> short 's')
  descriptionOption = strOption (long "description" <> short 'd')
  relationshipTypeOption = option auto (long "relationship-type" <> short 'r')
  targetTicketIDOption = strOption (long "target-ticket-id" <> short 't')
  create = fmap TicketAction $ CreateTicket <$> ticketIDArgument <*> (Ticket <$> nameOption <*> descriptionOption <*> statusOption) where
  editName = fmap TicketAction $ ChangeTicketName <$> ticketIDArgument <*> nameOption
  editStatus = fmap TicketAction $ ChangeTicketStatus <$> ticketIDArgument <*> statusOption
  editDescription = fmap TicketAction $ ChangeTicketDescription <$> ticketIDArgument <*> descriptionOption
  relate = fmap TicketAction $ CreateRelationship <$> ticketIDArgument <*> relationshipTypeOption <*> targetTicketIDOption
  unrelate = fmap TicketAction $ RemoveRelationship <$> ticketIDArgument <*> relationshipTypeOption <*> targetTicketIDOption
  get = fmap TicketQuery $ GetTicket <$> ticketIDArgument
  init = pure TicketInitialize
