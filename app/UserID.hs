module UserID
  ( UserID
  , makeRandomUserID
  ) where

import Data.UUID (UUID)
import Data.UUID.V4 (nextRandom)

type UserID = UUID

makeRandomUserID :: IO UserID
makeRandomUserID = nextRandom
