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
        CommandStatement cmd -> executeCommands filepath [cmd]
        QueryStatement q -> withTicketSystem filepath (mapM_ print . queryModel q . ticketModel)
        InitializeStatement -> do
          doesFileExist filepath >>= \case
            True -> fail "Trying to initialize a pre-existing ticket system"
            False -> BS.writeFile filepath (encode emptyTicketSystem)
        ValidateStatement -> do
          withTicketSystem filepath \ts -> do
            case appendCommands (ticketCommands ts) emptyTicketSystem of
              Nothing -> fail "Ticket system's commands are invalid"
              Just ts' -> do
                when (ts' /= ts) (fail "Ticket system's commands do not lead to the model present")
                putStrLn "Ticket system is valid"
  parser :: ParserInfo TicketStatement
  parser = flip info mods . hsubparser . mconcat $
    [ command "create" (info create (progDesc "Creates a new ticket"))
    , command "edit" (info edit (progDesc "Edits the name, description, and/or status of an existing ticket"))
    , command "relate" (info relate (progDesc "Relates two tickets to each other"))
    , command "unrelate" (info unrelate (progDesc "Removes the relationship between two tickets"))
    , command "query" (info query (progDesc "Search for tickets"))
    , command "init" (info initialize (progDesc "Initializes an empty ticket system"))
    , command "tag" (info tag (progDesc "Applies some tags to tickets"))
    , command "validate" (info validate (progDesc "Validate the ticket system"))
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
  filterOption = (FilterByName <$> nameOption) <|> (FilterByTag <$> tagOption) <|> (FilterByID <$> idOption) <|> (FilterByStatus <$> statusOption) <|> (FilterByRelationshipTo Blocks <$> blocksTargetOption) <|> (FilterByRelationshipTo Subsumes <$> subsumesTargetOption) <|> (FilterByRelationshipFrom Blocks <$> blockedByTargetOption) <|> (FilterByRelationshipFrom Subsumes <$> subsumedByTargetOption)
  blocksTargetOption = TicketID <$> strOption (long "blocks" <> short 'b')
  subsumesTargetOption = TicketID <$> strOption (long "subsumes" <> short 's')
  blockedByTargetOption = TicketID <$> strOption (long "blocked-by" <> short 'o')
  subsumedByTargetOption = TicketID <$> strOption (long "subsumed-by" <> short 'k')
  idOption = TicketID <$> strOption (long "id" <> short 'i')
  targetTicketIDOption = strOption (long "target-ticket-id" <> short 't')
  maybeOpt x = (Just <$> x) <|> pure Nothing
  create = fmap CommandStatement $ CreateTicket <$> ticketIDArgument <*> (Ticket <$> nameOption <*> descriptionOption <*> statusOption)
  edit = fmap CommandStatement $ ChangeTicket <$> ticketIDArgument <*> (TicketDiff <$> maybeOpt nameOption <*> maybeOpt descriptionOption <*> maybeOpt statusOption)
  relate = fmap CommandStatement $ CreateRelationship <$> ticketIDArgument <*> relationshipTypeOption <*> targetTicketIDOption
  unrelate = fmap CommandStatement $ RemoveRelationship <$> ticketIDArgument <*> relationshipTypeOption <*> targetTicketIDOption
  query = fmap QueryStatement $ Query <$> many filterOption <*> many queryOrderingOption <*> queryLimitOption
  initialize = pure InitializeStatement
  validate = pure ValidateStatement

byExample :: [(String, t)] -> ReadM t
byExample xs = maybeReader (\x -> Just (lookup x xs)) >>= maybe (readerAbort (ErrorMsg $ "Invalid ticket status, perhaps you meant to try one of: " <> intercalate ", " (fmap fst xs))) pure
