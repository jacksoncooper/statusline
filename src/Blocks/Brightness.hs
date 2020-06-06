module Blocks.Brightness
  ( brightnessBlock
  )
where

import Control.Applicative (liftA2)
import System.IO (FilePath, readFile)

import Block
import Parse

backlightPath :: String -> FilePath
backlightPath =
  ("/sys/class/backlight/intel_backlight/" ++)

brightnessPath :: FilePath
brightnessPath = backlightPath "actual_brightness"

maximumPath :: FilePath
maximumPath = backlightPath "max_brightness"

brightnessBlock :: Block
brightnessBlock =
  Block "brightness" "laptop" $ do
    maybeBrightness <- runParser parseDigits <$> readFile brightnessPath
    maybeMaximum    <- runParser parseDigits <$> readFile maximumPath
    
    let brightnessPair = liftA2 (,) maybeBrightness maybeMaximum

    return $
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
