import Test.QuickCheck
import Test.QuickCheck.Monadic

import System.FilePath ((</>))
import System.Directory (removeDirectoryRecursive)

import NekoFS (createNeko, extractNeko)
import Nekodata.Checksum (adler32ofFile)

outputDir :: FilePath
outputDir = "test/artifact/"

testFile :: FilePath
testFile = "test/cfg.nekodata"

testFile' :: FilePath
testFile' = outputDir </> "cfg1.nekodata"

testFile'' :: FilePath
testFile'' = outputDir </> "cfg2.nekodata"

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
  run $ removeDirectoryRecursive outputDir

main :: IO ()
main = do
  quickCheck prop_inv

