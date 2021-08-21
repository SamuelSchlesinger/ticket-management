{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
module TicketManager (main, program, runQuery, runCommands, runValidate, runInit, runGraphViz) where

import Data.Ticket
import Options.Applicative
import Imports
import qualified Data.Set as Set
import qualified Data.ByteString as BS
import Data.List (nub)
import Data.Aeson.TypeScript.TH (formatTSDeclarations', getTypeScriptDeclarations, FormattingOptions(..), ExportMode(ExportEach), SumTypeFormat(EnumWithType))

program' :: TicketStatement -> IO ()
program' ticketStatement = lookupEnv "TICKET_SYSTEM" >>= \case
  Nothing -> fail "No TICKET_SYSTEM environment variable set"
  Just filepath -> program filepath ticketStatement

runCommands :: FilePath -> [Command] -> IO ()
runCommands filepath cs = executeCommands filepath cs

runQuery :: FilePath -> Query -> IO [TicketDetails]
runQuery filepath q = withTicketSystem filepath (pure . queryModel q . ticketModel)

runValidate :: FilePath -> IO Bool
runValidate filepath = withTicketSystem filepath \ts -> do
  case appendCommands (ticketCommands ts) emptyTicketSystem of
    Nothing -> pure False
    Just ts' -> pure $ ts == ts'

runInit :: FilePath -> IO Bool
runInit filepath = doesFileExist filepath >>= \case
  True -> pure False
  False -> True <$ BS.writeFile filepath (encode emptyTicketSystem)

runGraphViz :: FilePath -> RelationshipType -> Query -> IO String
runGraphViz filepath rel q = withTicketSystem filepath (pure . graphViz q rel . ticketModel)

typeScriptFormattingOptions :: FormattingOptions
typeScriptFormattingOptions = FormattingOptions
  { numIndentSpaces = 2
  , interfaceNameModifier = id
  , typeNameModifier = id
  , exportMode = ExportEach
  , typeAlternativesFormat = EnumWithType
  }

program :: FilePath -> TicketStatement -> IO ()
program filepath ticketStatement = do
  let
    go td = do
      putStr (renderTicketDetails td)
      putStrLn (take 80 $ repeat '=')
  case ticketStatement of
    CommandStatement cmd -> runCommands filepath [cmd]
    QueryStatement q -> mapM_ go =<< runQuery filepath q
    InitializeStatement -> runInit filepath >>= \case
        True -> fail "Trying to initialize a pre-existing ticket system"
        False -> putStrLn "Ticket system initialized"
    GraphViz query rel -> runGraphViz filepath rel query >>= putStrLn
    ValidateStatement -> runValidate filepath >>= \case
          False -> fail "Ticket system's commands are invalid"
          True -> putStrLn "Ticket system is valid"
    TypeScript -> do
      putStrLn $ formatTSDeclarations' typeScriptFormattingOptions . nub . mconcat $ [
        getTypeScriptDeclarations (Proxy @TicketDetails),
        getTypeScriptDeclarations (Proxy @Query),
        getTypeScriptDeclarations (Proxy @Command),
        getTypeScriptDeclarations (Proxy @RelationshipType),
        getTypeScriptDeclarations (Proxy @TicketID),
        getTypeScriptDeclarations (Proxy @Ticket),
        getTypeScriptDeclarations (Proxy @TicketStatus),
        getTypeScriptDeclarations (Proxy @TicketDiff),
        getTypeScriptDeclarations (Proxy @Filter),
        getTypeScriptDeclarations (Proxy @Ordering),
        getTypeScriptDeclarations (Proxy @Limit),
        getTypeScriptDeclarations (Proxy @Ordering),
        getTypeScriptDeclarations (Proxy @Tag)
        ]

parser :: ParserInfo TicketStatement
parser = flip info mods . hsubparser . mconcat $
  [ command "create" (info create (progDesc "Create a new ticket"))
  , command "edit" (info edit (progDesc "Edit the name, description, and status of an existing ticket"))
  , command "relate" (info relate (progDesc "Relate one ticket to another"))
  , command "unrelate" (info unrelate (progDesc "Remove the relationship between two tickets"))
  , command "query" (info query (progDesc "Search for tickets"))
  , command "init" (info initialize (progDesc "Initializes an empty ticket system"))
  , command "tag" (info tag (progDesc "Applies some tags to tickets"))
  , command "validate" (info validate (progDesc "Validate the ticket system"))
  , command "graphviz" (info graphviz (progDesc "Output a dot formatted file describing a relation graph"))
  , command "typescript" (info typescript (progDesc "Output typescript declarations for the servant API"))
  ]
  where
    mods = header "Ticket Manager!" <> footer "Copyright 2021 (c) Samuel Schlesinger" <> progDesc "Allows the user to manage work tickets."
    tag = fmap CommandStatement $ CreateTags <$> ticketIDArgument "The ID of the ticket you wish to add tags to" <*> (Set.fromList <$> many tagOption)
    tagOption = strOption (long "tag" <> short 'x' <> metavar "TAG" <> help "The tag you wish to label the ticket with")
    ticketIDArgument h = strArgument (metavar "TICKET_ID" <> help h)
    nameOption = strOption (long "name" <> short 'n' <> metavar "NAME" <> help "The name of the ticket")
    statusOption = option statusReadM (long "status" <> short 's' <> metavar "STATUS" <> help "The status of the ticket")
    statusReadM = byExample
      [ ("todo", ToDo)
      , ("in-progress", InProgress)
      , ("in-review", InReview)
      , ("complete", Complete)
      , ("wont-fix", WontFix)
      ] 
    descriptionOption = strOption (long "description" <> short 'd' <> metavar "DESCRIPTION" <> help "The description of the ticket")
    queryOrderingReadM = byExample
      [("name", OrderByName)
      ,("id", OrderByID)
      ,("status", OrderByStatus)
      ]
    queryOrderingOption = option queryOrderingReadM (long "ordering" <> short 'o' <> metavar "ORDERING" <> help "The desired ordering of the resulting tickets (can use multiple). Can be name, id, or status.")
    queryLimitOption = option (Limit . Just <$> auto) (long "limit" <> short 'l' <> metavar "LIMIT" <> help "The maximum number of tickets to show" <> value (Limit Nothing))
    relationshipTypeOption = option relationshipTypeReadM (long "relationship-type" <> short 'r' <> metavar "RELATIONSHIP_TYPE" <> help "The type of the relationship to set. Can be blocks or subsumes.")
    relationshipTypeReadM = byExample
      [ ("blocks", Blocks)
      , ("subsumes", Subsumes)
      ]
    filterOption =
          (FilterByName <$> nameOption)
      <|> (FilterByTag <$> tagOption)
      <|> (FilterByID <$> idOption)
      <|> (FilterByStatus <$> statusOption)
      <|> (FilterByRelationshipTo Blocks <$> blocksTargetOption)
      <|> (FilterByRelationshipTo Subsumes <$> subsumesTargetOption)
      <|> (FilterByRelationshipFrom Blocks <$> blockedByTargetOption)
      <|> (FilterByRelationshipFrom Subsumes <$> subsumedByTargetOption)
    ticketOption l s h m = strOption (long l <> short s <> help h <> metavar m)
    blocksTargetOption = ticketOption "blocks" 'b' "Filters tickets to ones blocking the given ticket" "BLOCKS"
    subsumesTargetOption = ticketOption "subsumes" 's' "Filters tickets to ones subsuming the given ticket" "SUBSUMES"
    blockedByTargetOption = ticketOption "blocked-by" 'p' "Filters tickets to ones blocked by the given ticket" "BLOCKED_BY"
    subsumedByTargetOption = ticketOption "subsumed-by" 'k' "Filters tickets to ones subsumed by the given ticket" "SUBSUMED_BY"
    idOption = ticketOption "id" 'i' "Filters tickets to a single ID" "TICKET_ID"
    targetTicketIDOption = ticketOption "target-ticket-id" 't' "The ID of the ticket which is the target of the new relationship" "TARGET"
    maybeOpt x = (Just <$> x) <|> pure Nothing
    create = fmap CommandStatement $ CreateTicket <$> ticketIDArgument "An unused ID for the new ticket" <*> (Ticket <$> nameOption <*> descriptionOption <*> statusOption)
    edit = fmap CommandStatement $ ChangeTicket <$> ticketIDArgument "The ID of the ticket you want to edit" <*> (TicketDiff <$> maybeOpt nameOption <*> maybeOpt descriptionOption <*> maybeOpt statusOption)
    relate = fmap CommandStatement $ CreateRelationship <$> ticketIDArgument "The ID of the source ticket for the relationship" <*> relationshipTypeOption <*> targetTicketIDOption
    unrelate = fmap CommandStatement $ RemoveRelationship <$> ticketIDArgument "The ID of the source ticket for the relationship you wish to remove" <*> relationshipTypeOption <*> targetTicketIDOption
    queryBody = Query <$> many filterOption <*> many queryOrderingOption <*> queryLimitOption
    query = fmap QueryStatement queryBody
    initialize = pure InitializeStatement
    validate = pure ValidateStatement
    graphRelationOption = option relationshipTypeReadM (long "graph-relation" <> short 'g' <> metavar "GRAPH_RELATION" <> help "The relation we will output a graph for")
    graphviz = GraphViz <$> queryBody <*> graphRelationOption
    typescript = pure TypeScript

byExample :: [(String, t)] -> ReadM t
byExample xs = maybeReader (\x -> Just (lookup x xs)) >>= maybe (readerAbort (ErrorMsg $ "Invalid ticket status, perhaps you meant to try one of: " <> intercalate ", " (fmap fst xs))) pure

main :: IO ()
main = customExecParser ps parser >>= program' where
  ps :: ParserPrefs
  ps = prefs . mconcat $
    [ disambiguate
    , showHelpOnError
    , showHelpOnEmpty
    , columns 80
    ]
