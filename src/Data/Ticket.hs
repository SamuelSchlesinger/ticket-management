{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE NoImplicitPrelude #-}
module Data.Ticket where

import qualified Data.Map as Map
import qualified Data.Set as Set
import qualified Data.ByteString as BS
import qualified UnliftIO.IO.File as FileIO
import qualified Data.Aeson.TypeScript.TH as TS
import Data.Ord (Down(Down))
import Imports

-- | Execute the commands on the 'TicketSystem' stored in binary
-- format at the given file.
executeCommands :: FilePath -> [Command] -> IO ()
executeCommands filepath cs = withTicketSystem filepath \system -> appendCommands cs system >>= writeTicketSystem filepath

-- | Append the commands to the 'TicketSystem'.
appendCommands :: MonadFail m => [Command] -> TicketSystem -> m TicketSystem
appendCommands cs system = do
  newModel <- foldlM (flip stepTicketModel) (ticketModel system) cs
  let newCommands = cs <> ticketCommands system
  pure $ TicketSystem newCommands newModel
  
-- | Read the provided file for a 'TicketSystem' and run the function on the
-- 'TicketSystem'.
withTicketSystem :: FilePath -> (TicketSystem -> IO a) -> IO a
withTicketSystem filepath f = do
  decode <$> BS.readFile filepath >>= \case
    Left err -> fail err
    Right ticketSystem -> f ticketSystem

-- | Write the ticket system to a file.
writeTicketSystem :: FilePath -> TicketSystem -> IO ()
writeTicketSystem filepath ticketSystem = do
  FileIO.writeBinaryFileDurableAtomic filepath (encode ticketSystem)
 
-- | The sequence of commands which were executed on this ticket system, alongside
-- the model which is the result of executing these commands on the 'emptyTicketModel'.
data TicketSystem = TicketSystem
  { ticketCommands :: [Command]
  , ticketModel :: TicketModel
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Serialize)
  deriving (ToJSON, FromJSON) via Json TicketSystem

instance Arbitrary TicketSystem where
  arbitrary = do
    cs <- unValidCommandSequence <$> arbitrary
    case appendCommands cs emptyTicketSystem of
      Just system -> pure system
      Nothing -> error "Impossible"
  shrink ts = [ case appendCommands cs emptyTicketSystem of { Just system -> system; Nothing -> error "impossible" } | cs <- init $ inits (ticketCommands ts) ]

-- | A 'TicketSystem' with no history of commands or tickets.
emptyTicketSystem :: TicketSystem
emptyTicketSystem = TicketSystem
  { ticketCommands = []
  , ticketModel = emptyTicketModel
  }

-- | Evaluate the 'Command' on the 'TicketModel', producing another 'TicketModel'.
stepTicketModel :: MonadFail m => Command -> TicketModel -> m TicketModel
stepTicketModel cmd ts = case cmd of
  CreateTicket ticketID ticket ->
    createTicket ticketID ticket
  ChangeTicket ticketID ticketDiff ->
    modifyTicket ticketID ticketDiff
  CreateRelationship ticketID relationshipType ticketID' -> do
    when (ticketID == ticketID') (fail "cannot make relationship between ticket and itself")
    (ifExists "create-relationship-source-ticket" ticketID . ifExists "create-relationship-target-ticket" ticketID')
      (addRelationship ticketID relationshipType ticketID')
  RemoveRelationship ticketID relationshipType ticketID' ->
    (ifExists "remove-relationship-source-ticket" ticketID . ifExists "remove-relationship-target-ticket" ticketID')
      (removeRelationship ticketID relationshipType ticketID')
  CreateTags ticketID tgs ->
    (ifExists "create-tags-ticket" ticketID)
      (addTags ticketID tgs)
  RemoveTags ticketID tgs ->
    (ifExists "remove-tags-ticket" ticketID)
      (removeTags ticketID tgs)
  where
    removeTags ticketID tgs = do
      let
        go = maybe Nothing (Just . flip Set.difference tgs)
        newTags = Map.alter go ticketID (tags ts)
      pure $ ts { tags = newTags }
    addTags ticketID tgs = do
      let
        go = maybe (Just $ tgs) (Just . Set.union tgs) 
        newTags = Map.alter go ticketID (tags ts)
      pure $ ts { tags = newTags }
    ifExists :: MonadFail m => String -> TicketID -> m a -> m a
    ifExists msg ticketID ma = case Map.lookup ticketID (tickets ts) of
      Just _ -> ma
      Nothing -> fail ("Ticket " <> show ticketID <> " does not exist: " <> msg)
    createTicket ticketID ticket = 
      case Map.insertLookupWithKey keepNewValue ticketID ticket (tickets ts) of
        (Just _, _) -> fail "Attempted to create a ticket which already exists"
        (Nothing, tickets) -> pure $ ts { tickets }
    modifyTicket ticketID diff  =
      case Map.updateLookupWithKey (diffTicket diff) ticketID (tickets ts) of
        (Just _, tickets) -> pure $ ts { tickets }
        (Nothing, _) -> fail "Attempted to modify a ticket which doesn't exist"
    diffTicket diff _ticketID oldValue = Just $ oldValue { name = maybe (name oldValue) id (diffName diff), description = maybe (description oldValue) id (diffDescription diff), status = maybe (status oldValue) id (diffStatus diff) }
    keepNewValue _ticketID newValue _oldValue = newValue
    addRelationship ticketID relationshipType ticketID' = do
      let
        go' = maybe (Just $ Set.singleton ticketID') (Just . Set.insert ticketID')
        go = maybe (Just $ Map.singleton relationshipType (Set.singleton ticketID')) (Just . Map.alter go' relationshipType)
        relationships' = Map.alter go ticketID (relationships ts)
      pure $ ts { relationships = relationships' }
    removeRelationship ticketID relationshipType ticketID' = do
      let
        errMsg = Left "Attempted to delete relationship which does not exist"
        go'' = bool errMsg (Right False)
        go' = maybe errMsg (fmap Just . Set.alterF go'' ticketID')
        go = maybe errMsg (Right . Just <=< Map.alterF go' relationshipType)
      case Map.alterF go ticketID (relationships ts) of
        Left err -> fail err
        Right relationships -> pure $ ts { relationships }

-- | Expresses an edit to be made to a ticket's metadata.
data TicketDiff = TicketDiff
  { diffName :: Maybe String
  , diffDescription :: Maybe String
  , diffStatus :: Maybe TicketStatus
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Serialize)
  deriving (ToJSON, FromJSON) via Json TicketDiff

instance Arbitrary TicketDiff where
  arbitrary = TicketDiff <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

-- | A command to transform the 'TicketSystem'.
data Command =
    CreateTicket TicketID Ticket
    -- ^ Create the given ticket
  | ChangeTicket TicketID TicketDiff
    -- ^ Edit the given ticket
  | CreateRelationship TicketID RelationshipType TicketID
    -- ^ Create a relationship between two tickets
  | CreateTags TicketID (Set Tag)
    -- ^ Associate the given tags with the ticket
  | RemoveTags TicketID (Set Tag)
    -- ^ Remove the given tags from the ticket
  | RemoveRelationship TicketID RelationshipType TicketID
    -- ^ Remove a relationship between two tickets
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Serialize)
  deriving (ToJSON, FromJSON) via Json Command
instance Arbitrary Command where
  arbitrary = oneof
    [ CreateTicket <$> arbitrary <*> arbitrary
    , ChangeTicket <$> arbitrary <*> arbitrary
    , CreateRelationship <$> arbitrary <*> arbitrary <*> arbitrary
    , CreateTags <$> arbitrary <*> arbitrary
    , RemoveTags <$> arbitrary <*> arbitrary
    , RemoveRelationship <$> arbitrary <*> arbitrary <*> arbitrary
    ]

-- | Filter the result of executing a 'Query'.
data Filter =
    FilterByName String
    -- ^ Accept only tickets with this name
  | FilterByID TicketID
    -- ^ Accept only tickets with this ID
  | FilterByTag Tag
    -- ^ Accept only tickets with this tag
  | FilterByStatus TicketStatus
    -- ^ Accept only tickets with this status
  | FilterByRelationshipTo RelationshipType TicketID
    -- ^ Accept only tickets with the given relationship to the given ticket, with resulting tickets as the source of the relationship
  | FilterByRelationshipFrom RelationshipType TicketID
    -- ^ Accept only tickets with the given relationship to the given ticket, with resulting tickets as the target of the relationship
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Serialize)
  deriving (ToJSON, FromJSON) via Json Filter

-- | Order the result of executing a 'Query'.
data Ordering =
    OrderByName OrderingDirection
    -- ^ Order results by name
  | OrderByID OrderingDirection
    -- ^ Order results by ID
  | OrderByStatus OrderingDirection
    -- ^ Order results by status
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Serialize)
  deriving (ToJSON, FromJSON) via Json Ordering

-- | Order ascending or descending?
data OrderingDirection =
    Ascending
  | Descending
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Serialize)
  deriving (ToJSON, FromJSON) via Json OrderingDirection

-- | Limit the results of executing a 'Query'.
newtype Limit = Limit Int
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Serialize)
  deriving (ToJSON, FromJSON) via Json Limit

-- | A ticket query, allowing the user to search their 'TicketModel' for tickets
-- that satisfy a certain criteria.
data Query = Query
  { queryFilters :: [Filter]
  , queryOrderings :: [Ordering]
  , queryLimit :: Maybe Limit
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Serialize)
  deriving (ToJSON, FromJSON) via Json Query

-- | The details of a ticket, including not only its metadata but its ID, tags, and relationships.
data TicketDetails = TicketDetails
  { tdTicketID :: TicketID
  , tdTicket :: Ticket
  , tdTags :: [Tag]
  , tdRelationships :: [(RelationshipType, [TicketID])]
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Serialize)
  deriving (ToJSON, FromJSON) via Json TicketDetails

renderTicketDetails :: TicketDetails -> String
renderTicketDetails td = 
  "id: " <> (unTicketID $ tdTicketID td)
  <^> "name: " <> (name . tdTicket $ td)
  <^> "description: " <> (description . tdTicket $ td)
  <^> "tags: " <> (intercalate ", " $ map unTag (tdTags td))
  <^> "relationships:"
  <^> foldr (<^>) "" (map renderRelationships (tdRelationships td))
  where
    x <^> y = x <> "\n" <> y 
    spaces n = take n $ repeat ' '
    renderRelationships (rel, ts) =
      spaces 2 <> renderRelationship rel <> ": " <> (intercalate ", " $ map unTicketID ts)
    renderRelationship Blocks = "blocks"
    renderRelationship Subsumes = "subsumes"

-- | Return all of the tickets' details in the 'TicketModel'.
allTicketDetails :: TicketModel -> [TicketDetails]
allTicketDetails ts =
  let getTags tid = maybe [] Set.toList $ Map.lookup tid (tags ts)
      getRelationships tid = maybe [] (fmap (second Set.toList) . Map.toList) $ Map.lookup tid (relationships ts)
      transform tdTicketID tdTicket = TicketDetails { tdTicketID, tdTicket, tdTags = getTags tdTicketID, tdRelationships = getRelationships tdTicketID }
  in
    fmap (uncurry transform) $ Map.toList (tickets ts)

graphViz :: Query -> RelationshipType -> TicketModel -> String
graphViz query r ts = "strict digraph { \n" <> intercalate "\n  " (map (\(x, y) -> unTicketID x <> " -> " <> unTicketID y) rs) <> "\n}" where
  go as = [(x, y) | (x, ys) <- as, y <- Set.toList ys]
  xs = Set.fromList $ tdTicketID <$> queryModel query ts
  rs =
    go $ Map.toList $ Map.map
      (Set.filter (flip Set.member xs) . maybe Set.empty id . Map.lookup r)
      (Map.restrictKeys (relationships ts) xs)

-- | Execute a 'Query' on a 'TicketModel', resulting in a number of 'TicketDetails'
queryModel :: Query -> TicketModel -> [TicketDetails]
queryModel query ts = limit . order . filter' $ allTicketDetails ts where
  filter' :: [TicketDetails] -> [TicketDetails]
  filter' = appEndo $ foldMap filterFilter (queryFilters query) where
    filterFilter = Endo . filter . \case
      FilterByName n -> (== n) . name . tdTicket
      FilterByID tid -> (== tid) . tdTicketID
      FilterByStatus s -> (== s) . status . tdTicket
      FilterByTag tg -> \td ->
        case Map.lookup (tdTicketID td) (tags ts) of
          Just tgs -> Set.member tg tgs
          Nothing -> False
      FilterByRelationshipFrom rel tid -> \td ->
        case Map.lookup tid (relationships ts) of
          Just rs ->
            case Map.lookup rel rs of
              Just xs -> Set.member (tdTicketID td) xs
              Nothing -> False
          Nothing -> False
      FilterByRelationshipTo rel tid -> \td ->
        case Map.lookup (tdTicketID td) (relationships ts) of
          Just rs ->
            case Map.lookup rel rs of
              Just xs -> Set.member tid xs
              Nothing -> False
          Nothing -> False
  order :: [TicketDetails] -> [TicketDetails]
  order = appEndo $ foldMap orderingSort (queryOrderings query) where
    sortOnWithDir :: Ord a => OrderingDirection -> (TicketDetails -> a) -> [TicketDetails] -> [TicketDetails]
    sortOnWithDir = \case
      Ascending -> sortOn . (Down .)
      Descending -> sortOn
    orderingSort = Endo . \case
      OrderByName dir -> sortOnWithDir dir (name . tdTicket)
      OrderByID dir -> sortOnWithDir dir tdTicketID
      OrderByStatus dir -> sortOnWithDir dir (status . tdTicket)
  limit :: [TicketDetails] -> [TicketDetails]
  limit = case queryLimit query of
      Just (Limit n) -> take (fromIntegral n)
      Nothing -> id

-- | A statement which operates on the 'TicketModel'. These are what the user specifies
-- by a command line invocation.
data TicketStatement =
    CommandStatement Command
  | QueryStatement Query
  | InitializeStatement
  | ValidateStatement
  | GraphViz Query RelationshipType
  | TypeScript
  | Serve Int
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Serialize)
  deriving (ToJSON, FromJSON) via Json TicketStatement

-- | The status of a ticket.
data TicketStatus =
    ToDo
  | InProgress
  | InReview
  | Complete
  | WontFix
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving anyclass (Serialize)
  deriving (ToJSON, FromJSON) via Json TicketStatus

instance Arbitrary TicketStatus where
  arbitrary = elements [ minBound .. maxBound ]
  shrink = genericShrink

-- | Basic ticket metadata.
data Ticket = Ticket
  { name :: String
  , description :: String
  , status :: TicketStatus
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Serialize)
  deriving (ToJSON, FromJSON) via Json Ticket

instance Arbitrary Ticket where
  arbitrary = Ticket <$> arbitrary <*> arbitrary <*> arbitrary

-- | The ID of a 'Ticket'
newtype TicketID = TicketID { unTicketID :: String }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving newtype (Serialize, IsString, ToJSONKey, FromJSONKey)
  deriving (ToJSON, FromJSON) via Json TicketID

instance Arbitrary TicketID where
  arbitrary = TicketID <$> arbitrary
  shrink = genericShrink

-- | Arbitrary labels to associate with tickets
newtype Tag = Tag { unTag :: String }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving newtype (Serialize, IsString)
  deriving (ToJSON, FromJSON) via Json Tag

instance Arbitrary Tag where
  arbitrary = Tag <$> arbitrary
  shrink = genericShrink

-- | The various types of relationships which can be established between tickets
data RelationshipType =
    Blocks
  | Subsumes
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Serialize, ToJSONKey, FromJSONKey)
  deriving (ToJSON, FromJSON) via Json RelationshipType

instance ToHttpApiData RelationshipType where
  toUrlPiece = \case
    Blocks -> "blocks"
    Subsumes -> "subsumes"

instance FromHttpApiData RelationshipType where
  parseUrlPiece "blocks" = Right Blocks
  parseUrlPiece "subsumes" = Right Subsumes
  parseUrlPiece _ = Left "Try one of: blocks, subsumes"

instance Arbitrary RelationshipType where
  arbitrary = elements [Blocks, Subsumes]
  shrink = genericShrink

-- | The model of tickets, including their metadata, tags, and relationships.
data TicketModel = TicketModel
  { tickets :: Map TicketID Ticket
  , relationships :: Map TicketID (Map RelationshipType (Set TicketID))
  , tags :: Map TicketID (Set Tag)
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Serialize)
  deriving (ToJSON, FromJSON) via Json TicketModel

instance Arbitrary TicketModel where
  arbitrary = ticketModel <$> arbitrary
  shrink = genericShrink

-- | A newtype wrapper which has an 'Arbitrary' instance which generates valid command sequences,
-- which will not fail when appended to 'emptyTicketSystem'.
newtype ValidCommandSequence = ValidCommandSequence { unValidCommandSequence :: [Command] }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Serialize)
  deriving (ToJSON, FromJSON) via Json ValidCommandSequence

instance Arbitrary ValidCommandSequence where
  arbitrary = do
    xs <- arbitrary
    ValidCommandSequence <$> go xs emptyTicketSystem
    where
      go [] _ = pure []
      go (() : xs) ts = do
        (ts', c) <- suchThatMap arbitrary (\c -> ((, c) <$> appendCommands [c] ts))
        cs <- go xs ts'
        pure (c : cs)
  shrink (ValidCommandSequence commands) = fmap ValidCommandSequence $ init $ inits commands

-- | The empty ticket model
emptyTicketModel :: TicketModel
emptyTicketModel = TicketModel
  { tickets = Map.empty
  , relationships = Map.empty
  , tags = Map.empty
  }

$(TS.deriveTypeScript aesonOptions ''TicketDetails)
$(TS.deriveTypeScript aesonOptions ''Command)
$(TS.deriveTypeScript aesonOptions ''TicketID)
$(TS.deriveTypeScript aesonOptions ''Ticket)
$(TS.deriveTypeScript aesonOptions ''TicketStatus)
$(TS.deriveTypeScript aesonOptions ''TicketDiff)
$(TS.deriveTypeScript aesonOptions ''Tag)
$(TS.deriveTypeScript aesonOptions ''RelationshipType)
$(TS.deriveTypeScript aesonOptions ''Filter)
$(TS.deriveTypeScript aesonOptions ''Limit)
$(TS.deriveTypeScript aesonOptions ''OrderingDirection)
$(TS.deriveTypeScript aesonOptions ''Ordering)
$(TS.deriveTypeScript aesonOptions ''Query)
