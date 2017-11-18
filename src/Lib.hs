{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}

module Lib where

import Data.Int
import Data.List
import Data.Maybe (isJust, mapMaybe)
import qualified Data.Vector.Unboxed as VU
import Data.Binary
import Data.Binary.Get
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Builder as BB
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Search as BLS
import qualified Data.ByteString.Lazy.Char8 as BC
import Data.Function (on)
import Control.Monad (filterM)
import Control.Arrow (first)
import System.IO
import System.Environment
import Data.Monoid ((<>))
import System.Directory
import System.FilePath
import Control.Concurrent.Async (mapConcurrently_)


newtype EegFrame = MkEegFrame {
  samples :: VU.Vector Int16 }
              deriving(Eq)

instance Binary EegFrame where
  get = MkEegFrame <$> VU.replicateM 8 getInt16le
  put eegFrame = VU.mapM_ put (samples eegFrame)

data PatFrame = MkPatFrame {
  irLed  :: Int32,
  redLed :: Int32,
  accelerometer :: (Int16, Int16, Int16),
  termometer :: (Word8, Word8)
} deriving(Eq)

instance Binary PatFrame where
  put (MkPatFrame ir red (x, y, z) (t1, t2)) = put ir *> put red *> put x *> put y *> put z *> put t1 *> put t2
  get = MkPatFrame <$> getInt32le <*> getInt32le <*> ((,,) <$> getInt16le <*> getInt16le <*> getInt16le) <*> ((,) <$> get <*> get)

data InputFrame a = MkInputFrame Word32 a
  deriving(Eq, Read, Show)

instance (Binary a) => Binary (InputFrame a) where
  get = MkInputFrame <$> getWord32le <*> get
  put (MkInputFrame ts eegFrame) = put ts *> put eegFrame

data OutputFrame a = MkOutputFrame Int64 a
  deriving(Eq, Read, Show)


stringBuilder :: Show a => a -> Builder
stringBuilder = BB.stringUtf8 . show

commaAndStringBuilder :: Show a => a -> Builder
commaAndStringBuilder str = BB.charUtf8 ',' <> stringBuilder str

eegToCsv :: OutputFrame EegFrame -> Builder
eegToCsv (MkOutputFrame ts (MkEegFrame frs)) = VU.foldl buildLine mempty (VU.zip frameTimestamps frs)
  where buildLine acc (a, b) = acc <> BB.charUtf8 '\n' <> stringBuilder (ts+1000*a) <> commaAndStringBuilder b
        frameTimestamps = VU.enumFromStepN 0 8 56

eegCsvHeader :: Builder
eegCsvHeader = BB.byteString "timestamp,signal"

patCsvHeader :: Builder
patCsvHeader = BB.byteString "timestamp,ir_led,red_led,accel_x,accel_y,accel_z,temperature1,temperature2"

patToCsv :: OutputFrame PatFrame -> Builder
patToCsv (MkOutputFrame ts (MkPatFrame ir red (x, y, z) (t1, t2))) =
  mconcat [BB.charUtf8 '\n', stringBuilder ts, commaAndStringBuilder ir, commaAndStringBuilder red,
           commaAndStringBuilder x,  commaAndStringBuilder y,  commaAndStringBuilder  z,
           commaAndStringBuilder t1, commaAndStringBuilder t2]

rebuildTimestamps :: Int64 -> [InputFrame a] -> [OutputFrame a]
rebuildTimestamps _  [] = []
rebuildTimestamps ts (MkInputFrame delta0 fr : frs) =
  let diff = (ts - 1000 * fromIntegral delta0)
  in MkOutputFrame ts fr : rebuildRest diff frs
  where
    rebuildRest diff = map (\(MkInputFrame delta fr') ->
                              MkOutputFrame (diff + 1000 * fromIntegral delta) fr')

getStartTimestampFromCsv :: ByteString -> Int64
getStartTimestampFromCsv bts =
  let table = map (BC.split ',') $ BC.lines bts
      valueMap = zip (head table) (table !! 1)
      timestampBts = snd $ head $ filter (isJust . fst) $ map (first (findSubstring "StartDate")) valueMap
  in read $ BC.unpack timestampBts
  where
    findSubstring pat bts' = let (before, rest) = BLS.breakOn pat bts'
                            in if BL.length rest == 0 then Nothing
                               else Just $ BL.length before


data StreamInfo = EegStream | PatStream | Metadata
  deriving(Eq, Read, Show)

data FileInfo = MkFileInfo{ origName :: String, trialName :: String, dateString :: String, streamInfo :: StreamInfo, isSham :: Bool}
  deriving(Eq, Read, Show)

splitOn :: String -> String -> [String]
splitOn delims str = reverse $ worker [] "" str
  where worker acc curr []    = reverse curr : acc
        worker acc curr (n:r) = if n `elem` delims
                                then if curr /= "" then
                                       worker (reverse curr:acc) "" r
                                     else
                                       worker acc "" r
                                else
                                  worker acc (n : curr) r


parseFileInfo :: FilePath -> Maybe FileInfo
parseFileInfo str = parseFname (takeFileName str) (splitOn "-" (takeBaseName str ++ "-dummy"))

  where parseStream s
          | "eegstream" `isInfixOf` s = Just EegStream
          | "patstream" `isInfixOf` s = Just PatStream
          | "metadata"  `isInfixOf` s = Just Metadata
          | otherwise                 = Nothing

        parseFname :: String -> [String] -> Maybe FileInfo
        parseFname originalName (tname : dateStr : si : shamI : _) = do
          sInfo <- parseStream si
          return $ MkFileInfo originalName tname dateStr sInfo ("sham" `isInfixOf` shamI)
        parseFname _ _ = Nothing

parseProgram :: IO ()
parseProgram = getArgs >>= \args -> case length args of
      5 -> do
          let [eegIn, patIn, metaIn, eegOut, patOut] = args
          processTrial eegIn patIn (Just metaIn) eegOut patOut
      4 -> do
          let [eegIn, patIn, eegOut, patOut] = args
          processTrial eegIn patIn Nothing eegOut patOut
      0 -> processCurrentDirectory
      _ -> putStrLn "Need 5 or 0 arguments. [TODO]"

  where

    processCurrentDirectory  = do
      files <- getCurrentDirectory >>= getDirectoryContents >>= filterM (\x -> (not ("." `isPrefixOf` x) &&) <$> doesFileExist x)
      let fileInfos = mapMaybe parseFileInfo files
          fileGroups = groupBy ((==) `on` dateString) fileInfos
      print fileGroups
      mapConcurrently_ processGroup fileGroups

      where
        processGroup :: [FileInfo] -> IO ()
        processGroup grp = case adjustGroup grp of
          Nothing -> do
            putStr "Invalid group of files: "
            print grp
          Just as@(e, p, m) -> do
            print as
            let (eegOut, patOut) = outputNames e
              in processTrial (origName e) (origName p) (origName <$> m) eegOut patOut

        outputNames :: FileInfo -> (FilePath, FilePath)
        outputNames (MkFileInfo _ tname dateStr _ sham) =
          ( tname ++ "-" ++ dateStr ++ "-eegcsv" ++ (if sham then "-sham-" else "-") ++ "out.csv"
          , tname ++ "-" ++ dateStr ++ "-patcsv" ++ (if sham then "-sham-" else "-") ++ "out.csv")

        adjustGroup :: [FileInfo] -> Maybe (FileInfo,FileInfo,Maybe FileInfo)
        adjustGroup xs = do
          a <- safeHead $ filter ((==) EegStream . streamInfo) xs
          b <- safeHead $ filter ((==) PatStream . streamInfo) xs
          let c = safeHead $ filter ((==) Metadata  . streamInfo) xs
          return (a,b,c)

        safeHead []    = Nothing
        safeHead (x:_) = Just x

    processTrial eegIn patIn mMetaIn eegOut patOut = do

      testTimestamp <- case mMetaIn of
        Nothing     -> return 0
        Just metaIn -> getStartTimestampFromCsv <$> BC.readFile metaIn

      eegFrames <- rebuildTimestamps testTimestamp . parseEegFrames <$> BL.readFile eegIn
      patFrames <- rebuildTimestamps testTimestamp . parsePatFrames <$> BL.readFile patIn

      eegWriteHandle <- openWriteHandle eegOut
      patWriteHandle <- openWriteHandle patOut
      BB.hPutBuilder eegWriteHandle (mconcat $ eegCsvHeader : map eegToCsv eegFrames)
      BB.hPutBuilder patWriteHandle (mconcat $ patCsvHeader : map patToCsv patFrames)

        where
          openWriteHandle fname = do
            h <- openFile fname WriteMode
            hSetBuffering h LineBuffering
            return h

          parseEegFrames = map decode . chunked 20
          parsePatFrames = map decode . chunked 20

          chunked :: Int64 -> ByteString -> [ByteString]
          chunked n bs
            | BL.length bs < n = []
            | otherwise       = let (chunk,rest) = BL.splitAt n bs
                                in chunk : chunked n rest
