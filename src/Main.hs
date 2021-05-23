module Main where

import Options.Applicative

data Command
  = Add String String
  | Phone String String
  | Email String String
  | Show String
  deriving (Show, Eq)

parserAdd :: Parser Command
parserAdd =
  Add
    <$> strArgument (metavar "FILENAME")
    <*> strArgument (metavar "PERSON_NAME")

parserPhone :: Parser Command
parserPhone =
  Phone
    <$> strArgument (metavar "FILENAME")
    <*> strArgument (metavar "PHONE")

parserEmail :: Parser Command
parserEmail =
  Email
    <$> strArgument (metavar "FILENAME")
    <*> strArgument (metavar "EMAIL")

parserShow :: Parser Command
parserShow =
  Show
    <$> strArgument (metavar "FILENAME")

parser :: Parser Command
parser =
  subparser $
    command "add" (parserAdd `withInfo` "Add a new entry")
      <> command "phone" (parserPhone `withInfo` "Add a phone to an existing entry")
      <> command "email" (parserEmail `withInfo` "Add an email to an existing entry")
      <> command "show" (parserShow `withInfo` "Displays an existing entry")

withInfo :: Parser a -> String -> ParserInfo a
withInfo parser desc = info (helper <*> parser) $ progDesc desc

main :: IO ()
main = do
  command <- execParser $ parser `withInfo` "Manage the address book"
  print command