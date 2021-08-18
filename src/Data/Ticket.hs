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

appendCommands :: FilePath -> [Command] -> IO ()
appendCommands filepath actions = withTicketSystem filepath \system -> do
  newModel <- foldrM stepTicketModel (ticketModel system) actions
  let newCommands = actions <> ticketCommands system
  writeTicketSystem filepath $ system { ticketModel = newModel, ticketCommands = newCommands }
  
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

emptyTicketSystem :: TicketSystem
emptyTicketSystem = TicketSystem
  { ticketCommands = []
  , ticketModel = emptyTicketModel
  }

stepTicketModel :: MonadFail m => Command -> TicketModel -> m TicketModel
stepTicketModel cmd ts = case cmd of
  CreateTicket ticketID ticket ->
    createTicket ticketID ticket
  ChangeTicketName ticketID name ->
    modifyTicket ticketID changeName name
  ChangeTicketStatus ticketID ticketStatus ->
    modifyTicket ticketID changeStatus ticketStatus
  ChangeTicketDescription ticketID description ->
    modifyTicket ticketID changeDescription description
  CreateRelationship ticketID relationshipType ticketID' ->
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
    modifyTicket ticketID way change =
      case Map.updateLookupWithKey (way change) ticketID (tickets ts) of
        (Just _, tickets) -> pure $ ts { tickets }
        (Nothing, _) -> fail "Attempted to modify a ticket which doesn't exist"
    keepNewValue _ticketID newValue _oldValue = newValue
    changeName name _ticketID oldValue = Just $ oldValue { name }
    changeStatus status _ticketID oldValue = Just $ oldValue { status }
    changeDescription description _ticketID oldValue = Just $ oldValue { description }
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

data Command =
    CreateTicket TicketID Ticket
  | ChangeTicketName TicketID String
  | ChangeTicketStatus TicketID TicketStatus
  | ChangeTicketDescription TicketID String
  | CreateRelationship TicketID RelationshipType TicketID
  | CreateTags TicketID (Set Tag)
  | RemoveTags TicketID (Set Tag)
  | RemoveRelationship TicketID RelationshipType TicketID
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Serialize)

data Filter =
    Filter
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
  , queryContents :: QueryContents
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Serialize)

data QueryContents =
    GetTicket TicketID
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

underlyingQuery :: QueryContents -> TicketModel -> [TicketDetails]
underlyingQuery queryContents ts = case queryContents of
  GetTicket tdTicketID ->
    case Map.lookup tdTicketID (tickets ts) of
      Nothing -> []
      Just tdTicket ->
        let tdTags = maybe [] Set.toList $ Map.lookup tdTicketID (tags ts)
            tdRelationships = maybe [] (fmap (second Set.toList) . Map.toList) $ Map.lookup tdTicketID (relationships ts)
        in [TicketDetails {tdTicketID, tdTicket, tdTags, tdRelationships}]

queryModel :: Query -> TicketModel -> [TicketDetails]
queryModel query = limit . order . filter' . underlyingQuery (queryContents query) where
  filter' :: [TicketDetails] -> [TicketDetails]
  filter' = id
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
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Serialize)

data TicketStatus =
    ToDo
  | InProgress
  | InReview
  | Complete
  | WontFix
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Serialize)

data Ticket = Ticket
  { name :: String
  , description :: String
  , status :: TicketStatus
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Serialize)

newtype TicketID = TicketID { unTicketID :: String }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving newtype (Serialize, IsString)

newtype Tag = Tag { unTag :: String }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving newtype (Serialize, IsString)

data RelationshipType =
    Blocks
  | Subsumes
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Serialize)

data TicketModel = TicketModel
  { tickets :: Map TicketID Ticket
  , relationships :: Map TicketID (Map RelationshipType (Set TicketID))
  , tags :: Map TicketID (Set Tag)
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (Serialize)

emptyTicketModel :: TicketModel
emptyTicketModel = TicketModel
  { tickets = Map.empty
  , relationships = Map.empty
  , tags = Map.empty
  }
