{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module TicketManager (main) where

import Data.List (intercalate)
import Data.Ticket
import Options.Applicative
import Imports
import qualified Data.Set as Set
import qualified Data.ByteString as BS

main :: IO ()
main = customExecParser ps parser >>= program where
  ps :: ParserPrefs
  ps = prefs . mconcat $
    [ disambiguate
    , showHelpOnError
    , showHelpOnEmpty
    , columns 80
    ]
  program :: TicketStatement -> IO ()
  program ticketStatement = lookupEnv "TICKET_SYSTEM" >>= \case
    Nothing -> do
      fail "No TICKET_SYSTEM environment variable set"
    Just filepath -> do
      case ticketStatement of
        CommandStatement cmd -> appendCommands filepath [cmd]
        QueryStatement q -> withTicketSystem filepath (print . queryModel q . ticketModel)
        InitializeStatement -> do
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
    , command "init" (info initialize (progDesc "Initializes an empty ticket system"))
    , command "tag" (info tag (progDesc "Applies some tags to tickets"))
    ]
  mods = header "Ticket Manager!" <> footer "Copyright 2021 (c) Samuel Schlesinger" <> progDesc "Allows the user to manage work tickets."
  tag = fmap CommandStatement $ CreateTags <$> ticketIDArgument <*> (Set.fromList <$> many tagOption)
  tagOption = strOption (long "tag" <> short 'x')
  ticketIDArgument = strArgument (metavar "TICKET_ID")
  nameOption = strOption (long "name" <> short 'n')
  statusOption = option statusReadM (long "status" <> short 's')
  statusReadM = byExample
    [ ("todo", ToDo)
    , ("in-progress", InProgress)
    , ("in-review", InReview)
    , ("complete", Complete)
    , ("wont-fix", WontFix)
    ] 
  descriptionOption = strOption (long "description" <> short 'd')
  queryOrderingReadM = byExample
    [("name", OrderByName)
    ,("id", OrderByID)
    ,("status", OrderByStatus)
    ]
  queryOrderingOption = option queryOrderingReadM (long "ordering" <> short 'o')
  queryLimitOption = option (Limit . Just <$> auto) (long "limit" <> short 'l' <> value (Limit Nothing))
  relationshipTypeOption = option relationshipTypeReadM (long "relationship-type" <> short 'r')
  relationshipTypeReadM = byExample
    [ ("blocks", Blocks)
    , ("subsumes", Subsumes)
    ]
  targetTicketIDOption = strOption (long "target-ticket-id" <> short 't')
  create = fmap CommandStatement $ CreateTicket <$> ticketIDArgument <*> (Ticket <$> nameOption <*> descriptionOption <*> statusOption) where
  editName = fmap CommandStatement $ ChangeTicketName <$> ticketIDArgument <*> nameOption
  editStatus = fmap CommandStatement $ ChangeTicketStatus <$> ticketIDArgument <*> statusOption
  editDescription = fmap CommandStatement $ ChangeTicketDescription <$> ticketIDArgument <*> descriptionOption
  relate = fmap CommandStatement $ CreateRelationship <$> ticketIDArgument <*> relationshipTypeOption <*> targetTicketIDOption
  unrelate = fmap CommandStatement $ RemoveRelationship <$> ticketIDArgument <*> relationshipTypeOption <*> targetTicketIDOption
  query contents = fmap QueryStatement $ Query [] <$> many queryOrderingOption <*> queryLimitOption <*> contents
  get = query (GetTicket <$> ticketIDArgument)
  initialize = pure InitializeStatement

byExample :: [(String, t)] -> ReadM t
byExample xs = maybeReader (\x -> Just (lookup x xs)) >>= maybe (readerAbort (ErrorMsg $ "Invalid ticket status, perhaps you meant to try one of: " <> intercalate ", " (fmap fst xs))) pure
