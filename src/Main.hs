{-# LANGUAGE NamedFieldPuns #-}

module Main where

import Data.Char (toUpper)
import Options.Applicative
  ( argument,
    execParser,
    help,
    info,
    long,
    metavar,
    short,
    str,
    switch,
  )

data Welcome = Welcome
  { name :: String,
    excited :: Bool
  }

runWithOptions :: Welcome -> IO ()
runWithOptions Welcome {name, excited} =
  putStrLn $ transform ("Enjoy the snow, " ++ name ++ "!")
  where
    transform =
      if excited
        then map toUpper
        else id

main :: IO ()
main = execParser opts >>= runWithOptions
  where
    parser =
      Welcome
        <$> argument str (metavar "NAME")
        <*> switch
          ( short 'e'
              <> long "excited"
              <> help "Run in excited mode."
          )
    opts = info parser mempty
