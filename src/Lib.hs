{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE DeriveGeneric #-}

module Lib where

import Data.Int
import qualified Data.Vector.Unboxed as VU
import Data.Binary
import Data.Maybe (isJust)
import Data.Binary.Get
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Builder as BB
import Data.ByteString.Builder (Builder)
import qualified Data.ByteString.Lazy as BL
import qualified Data.ByteString.Lazy.Search as BLS
import qualified Data.ByteString.Lazy.Char8 as BC
import System.IO
import System.Environment
import Control.Exception
import Data.Monoid ((<>))
import GHC.Generics (Generic)


newtype EegFrame = MkEegFrame {
  samples :: VU.Vector Int16 }
              deriving(Eq, Read, Show, Generic)

instance Binary EegFrame where
  get = MkEegFrame <$> VU.replicateM 8 getInt16le
  put eegFrame = VU.mapM_ put (samples eegFrame)


data PatFrame = MkPatFrame {
  irLed  :: Int32,
  redLed :: Int32,
  accelerometer :: (Int16, Int16, Int16),
  termometer :: (Word8, Word8)
} deriving(Eq, Read, Show)

instance Binary PatFrame where
  put (MkPatFrame ir red (x, y, z) (t1, t2)) = put ir *> put red *> put x *> put y *> put z *> put t1 *> put t2
  get = MkPatFrame <$> getInt32le <*> getInt32le <*> ((,,) <$> getInt16le <*> getInt16le <*> getInt16le) <*> ((,) <$> get <*> get)

data InputFrame a = MkInputFrame Word32 a
  deriving(Eq, Read, Show, Generic)

instance Binary a => Binary (InputFrame a)

data OutputFrame a = MkOutputFrame Int64 a
  deriving(Eq, Read, Show, Generic)

instance Binary a => Binary (OutputFrame a)

chunked :: Int64 -> ByteString -> [ByteString]
chunked n bs
  | BL.length bs < n = []
  | otherwise       = let (chunk,rest) = BL.splitAt n bs
                      in chunk : chunked n rest

eegToCsv :: OutputFrame EegFrame -> Builder
eegToCsv (MkOutputFrame ts (MkEegFrame frs)) = VU.foldl buildLine mempty (VU.zip frameTimestamps frs)
  where buildLine acc (a, b) = acc <> BB.charUtf8 '\n' <> BB.stringUtf8 (show (ts+a)) <> BB.charUtf8 ',' <> (BB.stringUtf8 $ show b)
        frameTimestamps = VU.enumFromStepN 0 8 56

eegCsvHeader :: Builder
eegCsvHeader = BB.byteString "timestamp,signal"

patCsvHeader :: Builder
patCsvHeader = BB.byteString "timestamp,ir_led,red_led,accel_x,accel_y,accel_z,temperature1,temperature2"

patToCsv :: OutputFrame PatFrame -> Builder
patToCsv (MkOutputFrame ts (MkPatFrame ir red (x, y, z) (t1, t2))) =
  mconcat [BB.stringUtf8 $ show ts, BB.charUtf8 ',',  BB.stringUtf8 $ show ir, BB.charUtf8 ',', BB.stringUtf8 $ show red, BB.charUtf8 ',', BB.stringUtf8 $ show x,  BB.charUtf8 ',', BB.stringUtf8 $ show y,  BB.charUtf8 ',',  BB.stringUtf8 $ show  z,  BB.charUtf8 ',', BB.stringUtf8 $ show t1, BB.charUtf8 ',', BB.stringUtf8 $ show t2, BB.charUtf8 '\n']


rebuildTimestamps :: Int64 -> [InputFrame a] -> [OutputFrame a]
rebuildTimestamps _  [] = []
rebuildTimestamps ts ((MkInputFrame delta0 fr) : frs) =
  let diff = (ts - fromIntegral delta0)
  in (MkOutputFrame ts fr) : rebuildRest diff frs
  where
    rebuildRest diff = map (\(MkInputFrame delta fr') ->
                              MkOutputFrame (diff + fromIntegral delta) fr')


getStartTimestampFromCsv :: ByteString -> Int64
getStartTimestampFromCsv bts =
  let table = map (BC.split ',') $ BC.lines bts
      valueMap = zip (table !! 0) (table !! 1)
      timestampBts = snd $ head $ filter (isJust . fst) $ map (\(k,v) -> (findSubstring "starttimestamp" k, v)) valueMap
  in read $ BC.unpack $ timestampBts
  where
    findSubstring pat bts' = let (before, rest) = BLS.breakOn pat bts'
                            in if BL.length rest == 0 then Nothing
                               else Just $ BL.length before


parseProgram :: IO ()
parseProgram = do
  args <- getArgs

  -- testTimestamp <- (read <$> getLine) :: IO Word64
  testTimestamp <- exitWithMessageOnError "Invalid timestamp file structure" (getStartTimestampFromCsv <$> BC.readFile (args !! 2))

  eegFrames <- rebuildTimestamps testTimestamp . parseEegFrames <$> BL.readFile (args !! 0)
  patFrames <- rebuildTimestamps testTimestamp . parsePatFrames <$> BL.readFile (args !! 1)

  eegWriteHandle <- openWriteHandle (args !! 3)
  patWriteHandle <- openWriteHandle (args !! 4)
  BB.hPutBuilder eegWriteHandle (mconcat $ eegCsvHeader : map eegToCsv eegFrames)
  BB.hPutBuilder patWriteHandle (mconcat $ patCsvHeader : map patToCsv patFrames)

    where
      openWriteHandle fname = do
        h <- openFile fname WriteMode
        hSetBuffering h LineBuffering --(BlockBuffering Nothing)
        -- hSetBinaryMode h True
        return h

      parseEegFrames = map decode . chunked 20
      parsePatFrames = map decode . chunked 20

      exitWithMessageOnError :: String -> IO a -> IO a
      exitWithMessageOnError msg action = do
        eith <- try action
        case eith of
          Right x -> return x
          Left  e -> putStrLn msg >> ioError e
