module Blocks.Date
  ( dateBlock
  )
where

import Data.Time
  ( defaultTimeLocale
  , formatTime
  , getZonedTime
  )

import Block

readableDate :: IO String
readableDate =
  let zonedTime = getZonedTime
      formatString = "%A, %B %-e, %Y %l:%M:%S %p %Z"
  in formatTime defaultTimeLocale formatString <$> zonedTime

dateBlock :: Block
dateBlock =
  Block "date" "extended" readableDate
