module Main (main) where

import Data.Maybe (fromMaybe)
import Options.Applicative

import NekoFS

main :: IO ()
main = execParser opts >>= handleOptions
  where
    handleOptions (Extract file dest verify) = extractNeko file dest verify
    handleOptions (Create dir dest) = createNeko dir dest

-- parse command-line options

data Action
  = Extract FilePath -- ^ nekodata file
            FilePath -- ^ output directory
            Bool     -- ^ verify?
  | Create FilePath  -- ^ source directory
           FilePath  -- ^ output directory

extractOp :: Parser Action
extractOp = Extract <$> strOption
                         ( long "extract"
                        <> short 'x'
                        <> metavar "NEKOFILE"
                        <> help "The .nekodata file to extract" )
                    <*> outputDirP
                    <*> switch
                         ( long "verify"
                        <> short 'v'
                        <> help "verify integrity after extraction" )

createOp :: Parser Action
createOp = Create <$> strOption
                       ( long "create"
                      <> short 'c'
                      <> metavar "SRCDIR"
                      <> help "Source directory to be packed into a .nekodata file" )
                  <*> outputDirP

outputDirP :: Parser FilePath
outputDirP = fromMaybe "output" <$>
    optional (strOption
    (  metavar "OUTDIR"
    <> help "The output directory, default to 'output'" ))

opts :: ParserInfo Action
opts = info ( extractOp <|> createOp <**> helper)
  (  fullDesc
  <> header "nekofs: create and extract nekodata files")

