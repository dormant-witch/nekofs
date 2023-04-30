module Main (main) where

import Data.Maybe (fromMaybe)
import Options.Applicative

import NekoFS

main :: IO ()
main = execParser opts >>= handleOptions
  where
    handleOptions (Extract f, dest) = extractNeko f dest
    handleOptions (Create f, dest)  = createNeko f dest

-- parse command-line options

data Action
  = Extract FilePath
  | Create FilePath

type Options = (Action, FilePath)

extractOp :: Parser Action
extractOp = Extract <$> strOption
  (  long "extract"
  <> short 'x'
  <> metavar "NEKOFILE"
  <> help "The .nekodata file to extract" )

createOp :: Parser Action
createOp = Create <$> strOption
  (  long "create"
  <> short 'c'
  <> metavar "SRCDIR"
  <> help "Source directory to be packed into a .nekodata file" )

actionP :: Parser Action
actionP = extractOp <|> createOp

outputDirP :: Parser FilePath
outputDirP = fromMaybe "output" <$>
    optional (strOption
    (  metavar "OUTDIR"
    <> help "The output directory" ))

opts :: ParserInfo Options
opts = info ((,) <$>
      actionP <*> outputDirP <**> helper)
  (  fullDesc
  <> header "nekofs: create and extract nekodata files")

