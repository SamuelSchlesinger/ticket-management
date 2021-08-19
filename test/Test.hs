{-# LANGUAGE BlockArguments #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Data.Ticket
import Test.QuickCheck

args :: Args
args = stdArgs
  { maxSuccess = 100
  , chatty = True
  , maxSize = 6
  }

main :: IO ()
main = do
  quickCheckWith args \(unValidCommandSequence -> commands) system ->
    property $ maybe False (const True) $ appendCommands commands system
