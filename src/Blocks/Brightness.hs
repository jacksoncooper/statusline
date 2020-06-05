module Blocks.Brightness
  ( brightnessBlock
  )
where

import Control.Applicative (liftA2)
import System.IO (FilePath, readFile)
import Text.Printf (printf)

import Block
import Parse

backlightPath :: String -> FilePath
backlightPath =
  ("/sys/class/backlight/intel_backlight/" ++)

brightnessPath :: FilePath
brightnessPath = backlightPath "actual_brightness"

maxPath :: FilePath
maxPath = backlightPath "max_brightness"

parsePath :: FilePath -> IO (Result Int)
parsePath = fmap (runParser parseDigits . toState) . readFile

parsedBrightness :: IO (Result Int)
parsedBrightness = parsePath brightnessPath

parsedMax :: IO (Result Int)
parsedMax = parsePath maxPath

brightnessBlock :: Block
brightnessBlock =
  Block "brightness" "laptop" $
    parsedBrightness >>=
      \brightnessResult ->
        parsedMax >>=
          \maxResult ->
            return $
              let
                brightnessPair = liftA2 (,)
                  (getValue brightnessResult)
                  (getValue maxResult)
              in
                case brightnessPair of
                  Just (brightness, max) ->
                    let
                      numerator = fromIntegral brightness :: Double
                      denominator = fromIntegral max
                      ratio = numerator / denominator
                      percentage = ratio * 100
                    in
                         "Display: "
                      ++ show (round percentage)
                      ++ "%"
                  Nothing ->
                    "Failed to parse display brightness."
