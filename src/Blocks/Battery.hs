module Blocks.Battery
 ( batteryBlock
 )
where

import System.IO (FilePath, readFile)

import Block

batteryPath :: String -> FilePath
batteryPath battery =
     "/sys/class/power_supply/"
  ++ battery
  ++ "/capacity"

internalPath :: FilePath
internalPath = batteryPath "BAT0"

externalPath :: FilePath
externalPath = batteryPath "BAT1"

batteryBlock :: Block
batteryBlock =
  Block "battery" "combined" $
    readFile internalPath >>=
      \internalCapacity ->
        readFile externalPath >>=
          \externalCapacity ->
            return $
                 "Main: " ++ init internalCapacity ++ " "
              ++ "Extra: " ++ init externalCapacity
