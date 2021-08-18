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

appendTicketActions :: FilePath -> [TicketAction] -> IO ()
appendTicketActions filepath actions = withTicketSystem filepath \system -> do
  newModel <- foldrM stepTicketModel (ticketModel system) actions
  let newTicketActions = actions <> ticketActions system
  writeTicketSystem filepath $ system { ticketModel = newModel, ticketActions = newTicketActions }
  
withTicketSystem :: FilePath -> (TicketSystem -> IO a) -> IO a
withTicketSystem filepath f = do
  decode <$> BS.readFile filepath >>= \case
    Left err -> fail err
    Right ticketSystem -> f ticketSystem

writeTicketSystem :: FilePath -> TicketSystem -> IO ()
writeTicketSystem filepath ticketSystem = do
  FileIO.writeBinaryFileDurableAtomic filepath (encode ticketSystem)

ticketsByStatus :: (TicketStatus -> Bool) -> TicketModel -> [Ticket]
ticketsByStatus p ticketModel = filter (p . status) $ Map.elems (tickets ticketModel)

ticketsByName :: (String -> Bool) -> TicketModel -> [Ticket]
ticketsByName p ticketModel = filter (p . name) $ Map.elems (tickets ticketModel)

ticketsByDescription :: (String -> Bool) -> TicketModel -> [Ticket]
ticketsByDescription p ticketModel = filter (p . description) $ Map.elems (tickets ticketModel)
  
data TicketSystem = TicketSystem
  { ticketActions :: [TicketAction]
  , ticketModel :: TicketModel
  }
  deriving (Eq, Ord, Show, Read, Generic, Serialize)

stepTicketModel :: MonadFail m => TicketAction -> TicketModel -> m TicketModel
stepTicketModel cmd ticketModel = case cmd of
  CreateTicket ticketID ticket ->
    case Map.insertLookupWithKey keepNewValue ticketID ticket (tickets ticketModel) of
      (Just x, _) -> fail "Attempted to create a ticket which already exists"
      (Nothing, tickets) -> pure $ ticketModel { tickets }
  ChangeTicketName ticketID name -> modifyTicket ticketID changeName name
  ChangeTicketStatus ticketID ticketStatus -> modifyTicket ticketID changeStatus ticketStatus
  ChangeTicketDescription ticketID description -> modifyTicket ticketID changeDescription description
  CreateRelationship ticketID relationshipType ticketID' ->
    ifExists ticketID ticketModel . ifExists ticketID' ticketModel $ addRelationship ticketID relationshipType ticketID' ticketModel
  RemoveRelationship ticketID relationshipType ticketID' ->
    ifExists ticketID ticketModel . ifExists ticketID' ticketModel $ removeRelationship ticketID relationshipType ticketID' ticketModel
  where
    ifExists :: MonadFail m => TicketID -> TicketModel -> m a -> m a
    ifExists ticketID ticketModel ma = case Map.lookup ticketID (tickets ticketModel) of
      Just x -> ma
      Nothing -> fail "Ticket does not exist"
    modifyTicket ticketID way change =
      case Map.updateLookupWithKey (way change) ticketID (tickets ticketModel) of
        (Just x, tickets) -> pure $ ticketModel { tickets }
        (Nothing, _) -> fail "Attempted to modify a ticket which doesn't exist"
    keepNewValue _ticketID newValue _oldValue = newValue
    changeName name _ticketID oldValue = Just $ oldValue { name }
    changeStatus status _ticketID oldValue = Just $ oldValue { status }
    changeDescription description _ticketID oldValue = Just $ oldValue { description }
    addRelationship ticketID relationshipType ticketID' ticketModel = do
      let
        go' = \case
          Nothing -> Just $ Set.singleton ticketID'
          Just s -> Just $ Set.insert ticketID' s
        go = \case
          Nothing -> Just $ Map.singleton relationshipType (Set.singleton ticketID')
          Just m -> Just $ Map.alter go' relationshipType m
        relationships' = Map.alter go ticketID (relationships ticketModel)
      pure $ ticketModel { relationships = relationships' }
    removeRelationship ticketID relationshipType ticketID' ticketModel = do
      let
        errMsg = Left "Attempted to delete relationship which does not exist"
        go'' = \case
          True -> Right False
          False -> errMsg
        go' = \case
          Just s -> Just <$> Set.alterF go'' ticketID' s
          Nothing -> errMsg
        go = \case
          Just rs -> do
            rs' <- Map.alterF go' relationshipType rs
            Right (Just rs')
          Nothing -> errMsg
      case Map.alterF go ticketID (relationships ticketModel) of
        Left err -> fail err
        Right relationships -> pure $ ticketModel { relationships }

data TicketAction =
    CreateTicket TicketID Ticket
  | ChangeTicketName TicketID String
  | ChangeTicketStatus TicketID TicketStatus
  | ChangeTicketDescription TicketID String
  | CreateRelationship TicketID RelationshipType TicketID
  | RemoveRelationship TicketID RelationshipType TicketID
  deriving (Eq, Ord, Show, Read, Generic, Serialize)

data TicketStatus =
    ToDo
  | InProgress
  | InReview
  | Merging
  | Merged
  | WontFix
  deriving (Eq, Ord, Show, Read, Generic, Serialize)

data Ticket = Ticket
  { name :: String
  , description :: String
  , status :: TicketStatus
  }
  deriving (Eq, Ord, Show, Read, Generic, Serialize)

newtype TicketID = TicketID { unTicketID :: String }
  deriving (Eq, Ord, Show, Read, Generic, Serialize)

data RelationshipType =
    Blocks
  | Subsumes
  deriving (Eq, Ord, Show, Read, Generic, Serialize)

data TicketModel = TicketModel
  { tickets :: Map TicketID Ticket
  , relationships :: Map TicketID (Map RelationshipType (Set TicketID))
  }
  deriving (Eq, Ord, Show, Read, Generic, Serialize)
