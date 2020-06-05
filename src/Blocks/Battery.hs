module Blocks.Battery
 ( batteryBlock
 )
where

import System.IO (readFile)

import Block

batteryPath :: String -> String
batteryPath battery =
     "/sys/class/power_supply/"
  ++ battery
  ++ "/capacity"

internalPath :: String
internalPath = batteryPath "BAT0"

externalPath :: String
externalPath = batteryPath "BAT1"

batteryBlock :: Block
batteryBlock = Block "battery" "combined" $ 
  readFile internalPath >>=
    \internalCapacity ->
      readFile externalPath >>=
        \externalCapacity ->
          return $
               "Main: " ++ init internalCapacity ++ " "
            ++ "Extra: " ++ init externalCapacity

