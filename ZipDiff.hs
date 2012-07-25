import System.Environment
import Data.Word
import Data.Char
import Data.List
import qualified Codec.Archive.Zip as Zip
import qualified Data.ByteString.Lazy.Char8 as C8
import qualified Data.ByteString.Lazy as BS
import Diff

main :: IO ()
main = do
    (b:a:_) <- getArgs
    before <- BS.readFile b >>= return . zipinfos
    after  <- BS.readFile a >>= return . zipinfos
    mapM_ putFileDiff $ diff getPath before after

putFileDiff :: Diff FileInfo -> IO ()
putFileDiff f = case f of
    (Del f')   -> put "[DEL]" f'
    (Mod _ f') -> put "[MOD]" f'
    (Add f')   -> put "[ADD]" f'
    where put s f'' = putStr s >> C8.putStrLn (getPath f'')

data FileInfo = FileInfo {
    getPath :: BS.ByteString,
    getCRC32 :: Word32
} deriving (Show, Eq)

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
