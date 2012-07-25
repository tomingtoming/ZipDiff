import System.Environment
import Data.Word
import Data.Char
import Data.List
import qualified Codec.Archive.Zip as Zip
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.ByteString.Lazy as BS
import qualified Data.ByteString.Lazy.UTF8 as U

main :: IO ()
main = getArgs >>= BS.readFile . head >>= mapM_ C8.putStrLn . map show' . zipinfos

data FileInfo = FileInfo {
    getPath :: BS.ByteString,
    getCRC32 :: Word32
}

show' :: FileInfo -> BS.ByteString
show' f = U.fromString (show $ getCRC32 f) `BS.append` (U.fromString ":") `BS.append` (getPath f)

zipinfos :: BS.ByteString -> [FileInfo]
zipinfos = recEntries . Zip.zEntries . Zip.toArchive
    where recEntries :: [Zip.Entry] -> [FileInfo]
          recEntries [] = []
          recEntries (x:xs)
              | "/" `isSuffixOf` Zip.eRelativePath x      = recEntries xs
              | Zip.eRelativePath x `hasExtension` ".zip" = zipinfos (Zip.fromEntry x) ++ recEntries xs
              | otherwise                                 = zipinfo x : recEntries xs

zipinfo :: Zip.Entry -> FileInfo
zipinfo entry = FileInfo (Zip.eRawRelativePath entry) (Zip.eCRC32 entry)

hasExtension :: FilePath -> String -> Bool
hasExtension name ext = e `isSuffixOf` n
    where e = map toUpper ext
          n = map toUpper name
