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
import Imports

executeCommands :: FilePath -> [Command] -> IO ()
executeCommands filepath cs = withTicketSystem filepath \system -> appendCommands cs system >>= writeTicketSystem filepath

appendCommands :: MonadFail m => [Command] -> TicketSystem -> m TicketSystem
appendCommands cs system = do
  newModel <- foldrM stepTicketModel (ticketModel system) cs
  let newCommands = cs <> ticketCommands system
  pure $ TicketSystem newCommands newModel
  
withTicketSystem :: FilePath -> (TicketSystem -> IO a) -> IO a
withTicketSystem filepath f = do
  decode <$> BS.readFile filepath >>= \case
    Left err -> fail err
    Right ticketSystem -> f ticketSystem

writeTicketSystem :: FilePath -> TicketSystem -> IO ()
writeTicketSystem filepath ticketSystem = do
  FileIO.writeBinaryFileDurableAtomic filepath (encode ticketSystem)

ticketsByStatus :: (TicketStatus -> Bool) -> TicketModel -> [Ticket]
ticketsByStatus p ts = filter (p . status) $ Map.elems (tickets ts)

ticketsByName :: (String -> Bool) -> TicketModel -> [Ticket]
ticketsByName p ts = filter (p . name) $ Map.elems (tickets ts)

ticketsByDescription :: (String -> Bool) -> TicketModel -> [Ticket]
ticketsByDescription p ts = filter (p . description) $ Map.elems (tickets ts)
  
data TicketSystem = TicketSystem
  { ticketCommands :: [Command]
  , ticketModel :: TicketModel
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Serialize)

instance Arbitrary TicketSystem where
  arbitrary = do
    cs <- unValidCommandSequence <$> arbitrary
    case appendCommands cs emptyTicketSystem of
      Just system -> pure system
      Nothing -> error "Impossible"
  shrink ts = [ case appendCommands cs emptyTicketSystem of { Just system -> system; Nothing -> error "impossible" } | cs <- prefixes (ticketCommands ts) ]

emptyTicketSystem :: TicketSystem
emptyTicketSystem = TicketSystem
  { ticketCommands = []
  , ticketModel = emptyTicketModel
  }

stepTicketModel :: MonadFail m => Command -> TicketModel -> m TicketModel
stepTicketModel cmd ts = case cmd of
  CreateTicket ticketID ticket ->
    createTicket ticketID ticket
  ChangeTicket ticketID ticketDiff ->
    modifyTicket ticketID ticketDiff
  CreateRelationship ticketID relationshipType ticketID' -> do
    when (ticketID == ticketID') (fail "cannot make relationship between ticket and itself")
    (ifExists ticketID . ifExists ticketID')
      (addRelationship ticketID relationshipType ticketID')
  RemoveRelationship ticketID relationshipType ticketID' ->
    (ifExists ticketID . ifExists ticketID')
      (removeRelationship ticketID relationshipType ticketID')
  CreateTags ticketID tgs ->
    (ifExists ticketID)
      (addTags ticketID tgs)
  RemoveTags ticketID tgs ->
    (ifExists ticketID)
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
    ifExists :: MonadFail m => TicketID -> m a -> m a
    ifExists ticketID ma = case Map.lookup ticketID (tickets ts) of
      Just _ -> ma
      Nothing -> fail "Ticket does not exist"
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

data TicketDiff = TicketDiff
  { diffName :: Maybe String
  , diffDescription :: Maybe String
  , diffStatus :: Maybe TicketStatus
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Serialize)

instance Arbitrary TicketDiff where
  arbitrary = TicketDiff <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

data Command =
    CreateTicket TicketID Ticket
  | ChangeTicket TicketID TicketDiff
  | CreateRelationship TicketID RelationshipType TicketID
  | CreateTags TicketID (Set Tag)
  | RemoveTags TicketID (Set Tag)
  | RemoveRelationship TicketID RelationshipType TicketID
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Serialize)

instance Arbitrary Command where
  arbitrary = oneof
    [ CreateTicket <$> arbitrary <*> arbitrary
    , ChangeTicket <$> arbitrary <*> arbitrary
    , CreateRelationship <$> arbitrary <*> arbitrary <*> arbitrary
    , CreateTags <$> arbitrary <*> arbitrary
    , RemoveTags <$> arbitrary <*> arbitrary
    , RemoveRelationship <$> arbitrary <*> arbitrary <*> arbitrary
    ]

data Filter =
    FilterByName String
  | FilterByID TicketID
  | FilterByTag Tag
  | FilterByStatus TicketStatus
  | FilterByRelationshipTo RelationshipType TicketID
  | FilterByRelationshipFrom RelationshipType TicketID
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Serialize)

data Ordering =
    OrderByName
  | OrderByID
  | OrderByStatus
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Serialize)

data Limit = Limit (Maybe Word)
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Serialize)

data Query = Query
  { queryFilters :: [Filter]
  , queryOrderings :: [Ordering]
  , queryLimit :: Limit
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Serialize)

data TicketDetails = TicketDetails
  { tdTicketID :: TicketID
  , tdTicket :: Ticket
  , tdTags :: [Tag]
  , tdRelationships :: [(RelationshipType, [TicketID])]
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Serialize)

allTicketDetails :: TicketModel -> [TicketDetails]
allTicketDetails ts =
  let getTags tid = maybe [] Set.toList $ Map.lookup tid (tags ts)
      getRelationships tid = maybe [] (fmap (second Set.toList) . Map.toList) $ Map.lookup tid (relationships ts)
      transform tdTicketID tdTicket = TicketDetails { tdTicketID, tdTicket, tdTags = getTags tdTicketID, tdRelationships = getRelationships tdTicketID }
  in
    fmap (uncurry transform) $ Map.toList (tickets ts)

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
    orderingSort = Endo . \case
      OrderByName -> sortOn (name . tdTicket)
      OrderByID -> sortOn tdTicketID
      OrderByStatus -> sortOn (status . tdTicket)
  limit :: [TicketDetails] -> [TicketDetails]
  limit = case queryLimit query of
      Limit (Just n) -> take (fromIntegral n)
      Limit Nothing -> id

getTicket :: MonadFail m => TicketID -> TicketModel -> m Ticket
getTicket ticketID ts =
  case Map.lookup ticketID (tickets ts) of
    Just x -> pure x
    Nothing -> fail "Could not find ticket"

data TicketStatement =
    CommandStatement Command
  | QueryStatement Query
  | InitializeStatement
  | ValidateStatement
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Serialize)

data TicketStatus =
    ToDo
  | InProgress
  | InReview
  | Complete
  | WontFix
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving anyclass (Serialize)

instance Arbitrary TicketStatus where
  arbitrary = elements [ minBound .. maxBound ]
  shrink = genericShrink

data Ticket = Ticket
  { name :: String
  , description :: String
  , status :: TicketStatus
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Serialize)

instance Arbitrary Ticket where
  arbitrary = Ticket <$> arbitrary <*> arbitrary <*> arbitrary

newtype TicketID = TicketID { unTicketID :: String }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving newtype (Serialize, IsString)

instance Arbitrary TicketID where
  arbitrary = TicketID <$> arbitrary
  shrink = genericShrink

newtype Tag = Tag { unTag :: String }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving newtype (Serialize, IsString)

instance Arbitrary Tag where
  arbitrary = Tag <$> arbitrary
  shrink = genericShrink

data RelationshipType =
    Blocks
  | Subsumes
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Serialize)

instance Arbitrary RelationshipType where
  arbitrary = elements [Blocks, Subsumes]
  shrink = genericShrink

data TicketModel = TicketModel
  { tickets :: Map TicketID Ticket
  , relationships :: Map TicketID (Map RelationshipType (Set TicketID))
  , tags :: Map TicketID (Set Tag)
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Serialize)

instance Arbitrary TicketModel where
  arbitrary = TicketModel <$> arbitrary <*> arbitrary <*> arbitrary
  shrink = genericShrink

newtype ValidCommandSequence = ValidCommandSequence { unValidCommandSequence :: [Command] }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Serialize)

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
  shrink (ValidCommandSequence commands) = fmap ValidCommandSequence $ prefixes commands

prefixes :: [a] -> [[a]]
prefixes as@(_ : as') = as : prefixes as'
prefixes [] = [] : []

emptyTicketModel :: TicketModel
emptyTicketModel = TicketModel
  { tickets = Map.empty
  , relationships = Map.empty
  , tags = Map.empty
  }
