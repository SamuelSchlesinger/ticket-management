{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Data.Ticket
import Test.QuickCheck

args :: Args
args = stdArgs
  { maxSuccess = 1000
  , chatty = True
  }

main :: IO ()
main = do
  quickCheckWith args \(unValidCommandSequence -> commands) system ->
    property $ maybe False (const True) $ appendCommands commands system
