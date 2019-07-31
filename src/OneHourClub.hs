module OneHourClub (isClosed) where

import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Zones
import Data.Time.Zones.All (tzByLabel, TZLabel(Europe__London))

currentLocalTimeByLabel :: TZ -> IO LocalTime
currentLocalTimeByLabel localTZ = do
  currentTime <- getCurrentTime
  pure $ utcToLocalTimeTZ localTZ currentTime

startTimeOfDay :: TimeOfDay
startTimeOfDay = TimeOfDay { todHour = 6, todMin = 0, todSec = 0 }

endTimeOfDay :: TimeOfDay
endTimeOfDay = TimeOfDay { todHour = 7, todMin = 0, todSec = 0 }

timeOfDayIsOpen :: TimeOfDay -> Bool
timeOfDayIsOpen tod
  | tod < startTimeOfDay = False
  | tod > endTimeOfDay   = False
  | otherwise            = True

isClosed :: IO Bool
isClosed = do
  let localTZ = tzByLabel Europe__London
  LocalTime { localTimeOfDay = currentTimeOfDay } <-
    currentLocalTimeByLabel localTZ
  pure $ not $ timeOfDayIsOpen currentTimeOfDay

