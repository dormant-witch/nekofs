import Test.QuickCheck
import Test.QuickCheck.Monadic

import System.FilePath ((</>))

import NekoFS (createNeko, extractNeko)
import Nekodata.Checksum (adler32ofFile)

baseDir :: FilePath
baseDir = "test/artifact/"

outputDir :: FilePath
outputDir = baseDir </> "output/"

testFile :: FilePath
testFile = baseDir </> "cfg.nekodata"

testFile' :: FilePath
testFile' = baseDir </> "cfg1.nekodata"

testFile'' :: FilePath
testFile'' = baseDir </> "cfg2.nekodata"

prop_inv :: Property
prop_inv = once $ monadicIO $ do
  run $ do
    extractNeko testFile outputDir False
    createNeko (outputDir </> "cfg/") testFile'
    extractNeko testFile' outputDir False
    createNeko (outputDir </> "cfg1/") testFile''

  chksum'  <- run $ adler32ofFile testFile'
  chksum'' <- run $ adler32ofFile testFile''
  assert $ chksum' == chksum''

main :: IO ()
main = do
  quickCheck prop_inv

