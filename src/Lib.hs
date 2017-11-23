{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings     #-}
{-# LANGUAGE RankNTypes            #-}
{-# LANGUAGE StrictData            #-}

module Lib(
  parseProgram
  ) where

import           Control.Arrow               (first)
import           Control.Concurrent.Async    (concurrently, mapConcurrently_)
import           Control.Monad               (filterM, forever)
import           Data.Binary
import           Data.Binary.Get
import           Data.ByteString.Builder     (Builder)
import qualified Data.ByteString.Builder     as BB
import qualified Data.ByteString.Lazy        as BL
import qualified Data.ByteString.Lazy.Char8  as BC
import qualified Data.ByteString.Lazy.Search as BLS
import           Data.Function               (on)
import           Data.Int
import           Data.List
import           Data.Maybe                  (isJust, mapMaybe)
import           Data.Monoid                 ((<>))
import qualified Data.Vector.Unboxed         as VU
import           Pipes
import qualified Pipes.ByteString            as P
import qualified Pipes.Prelude               as PP
import           System.Directory
import           System.Environment
import           System.FilePath
import           System.IO


newtype EegFrame = MkEegFrame {
  samples :: VU.Vector Int16 }
              deriving(Eq)

instance Binary EegFrame where
  get = MkEegFrame <$> VU.replicateM 8 getInt16le
  put eegFrame = VU.mapM_ put (samples eegFrame)

data PatFrame = MkPatFrame {
  irLed         :: Int32,
  redLed        :: Int32,
  accelerometer :: (Int16, Int16, Int16),
  termometer    :: (Word8, Word8)
} deriving(Eq)

instance Binary PatFrame where
  put (MkPatFrame ir red (x, y, z) (t1, t2)) = put ir *> put red *> put x *> put y *> put z *> put t1 *> put t2
  get = MkPatFrame <$> getInt32le <*> getInt32le <*> ((,,) <$> getInt16le <*> getInt16le <*> getInt16le) <*> ((,) <$> get <*> get)

data InputFrame a = MkInputFrame Word32 a
  deriving(Eq, Read, Show)

instance (Binary a) => Binary (InputFrame a) where
  get = MkInputFrame <$> getWord32le <*> get
  put (MkInputFrame ts fr) = put ts *> put fr

data OutputFrame a = MkOutputFrame Int64 a
  deriving(Eq, Read, Show)


stringBuilder :: Show a => a -> Builder
stringBuilder = BB.stringUtf8 . show

commaAndStringBuilder :: Show a => a -> Builder
commaAndStringBuilder str = BB.charUtf8 ',' <> stringBuilder str

eegToCsv :: OutputFrame EegFrame -> Builder
eegToCsv (MkOutputFrame ts (MkEegFrame frs)) = VU.foldl buildLine mempty (VU.zip frameTimestamps frs)
  where buildLine acc (a, b) = acc <> BB.charUtf8 '\n' <> stringBuilder (ts+1000*a) <> commaAndStringBuilder b
        frameTimestamps = VU.enumFromStepN 0 8 8

eegCsvHeader :: Builder
eegCsvHeader = BB.byteString "timestamp,signal"

patCsvHeader :: Builder
patCsvHeader = BB.byteString "timestamp,ir_led,red_led,accel_x,accel_y,accel_z,temperature1,temperature2"

patToCsv :: OutputFrame PatFrame -> Builder
patToCsv (MkOutputFrame ts (MkPatFrame ir red (x, y, z) (t1, t2))) =
  mconcat [BB.charUtf8 '\n', stringBuilder ts, commaAndStringBuilder ir, commaAndStringBuilder red,
           commaAndStringBuilder x,  commaAndStringBuilder y,  commaAndStringBuilder  z,
           commaAndStringBuilder t1, commaAndStringBuilder t2]

getStartTimestampFromCsv :: BC.ByteString -> Int64
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
          return $ MkFileInfo originalName tname dateStr sInfo ("sham" `isInfixOf` shamI && not ("nosham" `isInfixOf` shamI))
        parseFname _ _ = Nothing

parseProgram :: IO ()
parseProgram = getArgs >>= \args -> case length args of
      5 -> do
          let [eegIn, patIn, metaIn, eegOut, patOut] = args
          processTrialPipes eegIn patIn (Just metaIn) eegOut patOut
      4 -> do
          let [eegIn, patIn, eegOut, patOut] = args
          processTrialPipes eegIn patIn Nothing eegOut patOut
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
              in processTrialPipes (origName e) (origName p) (origName <$> m) eegOut patOut

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

    processTrialPipes eegIn patIn mMetaIn eegOut patOut = do

      startTs <- case mMetaIn of
        Nothing     -> return 0
        Just metaIn -> getStartTimestampFromCsv <$> BC.readFile metaIn

      void $ concurrently
        (parseFrames startTs eegIn eegOut eegToCsv eegCsvHeader)
        (parseFrames startTs patIn patOut patToCsv patCsvHeader)

        where
          parseFrames startTs fIn fOut parseF header = withFile fIn ReadMode  $ \hIn  -> do
            hSetBinaryMode hIn True
            hSetBuffering hIn (BlockBuffering (Just 20))
            withFile fOut WriteMode $ \hOut -> do
              hSetBuffering hOut LineBuffering
              BB.hPutBuilder hOut header
              runEffect $ P.hGet 20 hIn >-> decodeBinaryFrame >-> rebuildTimestampsPipe startTs >-> PP.map parseF >-> PP.mapM_ (BB.hPutBuilder hOut)

          decodeBinaryFrame :: forall m a. (Monad m, Binary a) => Pipe P.ByteString (InputFrame a) m ()
          decodeBinaryFrame = forever $ await >>= yield . decode . BC.fromStrict

          rebuildTimestampsPipe :: forall m a. (Monad m) => Int64 -> Pipe (InputFrame a) (OutputFrame a) m ()
          rebuildTimestampsPipe ts = do
            (MkInputFrame delta0 fr) <- await
            let diff = (ts - 1000 * fromIntegral delta0)
            yield $ MkOutputFrame ts fr
            forever $ do
              (MkInputFrame delta fr') <- await
              yield $ MkOutputFrame (diff + 1000 * fromIntegral delta) fr'
