{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Data.Ticket
import Test.QuickCheck

args :: Args
args = stdArgs
  { maxSuccess = 10000
  , chatty = True
  , maxSize = 10000
  }

main :: IO ()
main = do
  quickCheckWith args \(unValidCommandSequence -> commands) ->
    property $ maybe False (const True) $ appendCommands commands emptyTicketSystem
  quickCheckWith args \system ->
    property $ appendCommands (ticketCommands system) emptyTicketSystem == Just system
