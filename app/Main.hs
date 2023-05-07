module Main (main) where

import Options.Applicative

import NekoFS

main :: IO ()
main = execParser opts >>= handleOptions
  where
    handleOptions (Extract file dest verify) = extractNeko file dest verify
    handleOptions (Create dir dest) = createNeko dir dest
    handleOptions (GenMeta path) = generateMeta path

-- parse command-line options

data Action
  = Extract FilePath -- ^ nekodata file
            FilePath -- ^ output directory
            Bool     -- ^ verify?
  | Create FilePath  -- ^ source directory
           FilePath  -- ^ output filename
  | GenMeta FilePath -- ^ file or directory

extractOp :: Parser Action
extractOp = Extract <$> strOption
                         ( long "extract"
                        <> short 'x'
                        <> metavar "NEKOFILE"
                        <> help "The .nekodata file to extract" )
                    <*> argument str
                         ( metavar "OUTDIR"
                        <> help "The output directory"
                        <> showDefault
                        <> value "output")
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
                  <*> argument str
                       ( metavar "OUTFILE"
                      <> help "The output file name"
                      <> showDefault
                      <> value "SRCDIR.nekodata")

genMetaOp :: Parser Action
genMetaOp = GenMeta <$> strOption
                       ( long "meta"
                      <> short 'm'
                      <> metavar "FILEPATH"
                      <> help "Generate \"files.meta\" for this directory or file" )

opts :: ParserInfo Action
opts = info ( extractOp <|> createOp <|> genMetaOp <**> helper)
  (  fullDesc
  <> header "nekofs: create and extract nekodata files")

