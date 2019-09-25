-- Module for helper functions

module Helpers
  (getRandomUUID) where

import qualified Data.UUID as UUID
import qualified System.Random as Rand

-- getRandomUUID :: UUID.UUID

getRandomUUID = do
  uuid <- Rand.randomIO :: IO UUID.UUID
  print uuid
