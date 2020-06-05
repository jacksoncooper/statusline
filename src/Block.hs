module Block
  ( Block (..)
  , information
  )
where

-- The fields of the Block data constructor mirror those defined by 'swaybar-protocol':
-- https://manpages.debian.org/experimental/sway/swaybar-protocol.7.en.html

data Block = Block
  { kind :: String
  , identifier :: String
  , getFullText :: IO String
  }

information :: Block -> IO ()
information block =
  getFullText block >>=
    \fullText ->
         putStrLn ("Name: " ++ kind block)
      >> putStrLn ("Instance: " ++ identifier block)
      >> putStrLn ("Full text: " ++ fullText)
