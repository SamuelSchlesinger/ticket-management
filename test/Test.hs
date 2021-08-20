{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Data.Bool (bool)
import Data.Ticket
import Test.QuickCheck
import System.Exit (exitFailure)
import System.IO (hPutStrLn, stderr)

args :: Args
args = stdArgs
  { maxSuccess = 1000
  , chatty = True
  , maxSize = 1000
  }

require :: Bool -> String -> IO ()
require t msg = if t then pure () else hPutStrLn stderr msg >> exitFailure

commandsInvalid :: [Command] -> Bool
commandsInvalid = testCommands (maybe True (const False))

commandsValid :: [Command] -> Bool
commandsValid = testCommands (maybe False (const True))

testCommands :: (Maybe TicketSystem -> Bool) -> [Command] -> Bool
testCommands f cs = f $ appendCommands cs emptyTicketSystem

main :: IO ()
main = do
  -- Make sure the valid command sequence generator generates valid command sequences
  check "ValidCommandSequences are valid" \(unValidCommandSequence -> commands) ->
    property $ maybe False (const True) $ appendCommands commands emptyTicketSystem
  -- Make sure the system generator produces valid systems
  check "arbitrary ticket systems are valid" \system ->
    property $ appendCommands (ticketCommands system) emptyTicketSystem == Just system
  -- Make sure we can't create tickets with duplicate ids
  check "no ticket ID overlap" \ticketID1 ticket1 ticket2 ->
    property $ commandsInvalid [CreateTicket ticketID1 ticket1, CreateTicket ticketID1 ticket2]
  -- Check edit command
  check "change" \ticketID1 ticket1 ticketDiff ->
    property $ commandsValid [CreateTicket ticketID1 ticket1, ChangeTicket ticketID1 ticketDiff]
  -- Check relationship commands
  check "relate"  \ticketID1 ticket1 ticket2 rel -> property do
    ticketID2 <- suchThat arbitrary (/= ticketID1)
    pure $ commandsValid
      [ CreateTicket ticketID1 ticket1, CreateTicket ticketID2 ticket2
      , CreateRelationship ticketID1 rel ticketID2
      ]
  where
    check :: Testable prop => String -> prop -> IO ()
    check msg prop = do
      putStrLn msg
      isSuccess <$> quickCheckWithResult args prop >>= bool exitFailure (pure ())
