{-# LANGUAGE CPP                   #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE TypeFamilies          #-}

module Data.Time.Clock.TAI.TAI64
    ( TAI64      (taiSecs, taiNanos, taiAttos)
    , TAI64Label (..)

    , tai1970

    , toAbsoluteTime
    , fromAbsoluteTime
    , toUTCTime
    , fromUTCTime

    , toText
    , fromText
    , toByteString
    , fromByteString

    , parse
    , parseText
    , parseByteString
    )
where

import           Control.Applicative
import           Control.Monad                    (liftM)
import qualified Data.Attoparsec.ByteString.Char8 as PB
import           Data.Attoparsec.Combinator       (option)
import           Data.Attoparsec.Internal.Types   (Parser)
import qualified Data.Attoparsec.Text             as PT
import           Data.Binary
import           Data.Bits
import           Data.ByteString                  (ByteString)
import qualified Data.ByteString.Builder          as BB
import qualified Data.ByteString.Lazy             as BL
import           Data.Text                        (Text)
import qualified Data.Text.Lazy                   as TL
import qualified Data.Text.Lazy.Builder           as TB
import qualified Data.Text.Lazy.Builder.Int       as TB
import           Data.Time.Clock
import           Data.Time.Clock.TAI
import qualified Data.Vector.Generic              as VG
import qualified Data.Vector.Generic.Mutable      as VM
import           Data.Vector.Unboxed.Base


data TAI64 = TAI64
    { taiSecs  :: {-# UNPACK #-} !Word64
    , taiNanos :: {-# UNPACK #-} !Word32
    , taiAttos :: {-# UNPACK #-} !Word32
    } deriving (Eq, Show, Ord)

data TAI64Label
    = TAI64S  {-# UNPACK #-} !TAI64
    | TAI64N  {-# UNPACK #-} !TAI64
    | TAI64NA {-# UNPACK #-} !TAI64
    deriving (Eq, Show, Ord)

instance Binary TAI64Label where
    put (TAI64S  tai) = put (taiSecs tai, taiNanos tai)
    put (TAI64N  tai) = put (taiSecs tai, taiNanos tai)
    put (TAI64NA tai) = put (taiSecs tai, taiNanos tai, taiAttos tai)

    get = do
        elts <- (,,) <$> get <*> optional get <*> optional get
        pure $ case elts of
            (s, Just  n, Just  a) -> TAI64NA (TAI64 s n a)
            (s, Just  n, Nothing) -> TAI64N  (TAI64 s n 0)
            (s, Nothing, Nothing) -> TAI64S  (TAI64 s 0 0)
            (s, Nothing, Just  n) -> TAI64N  (TAI64 s n 0)


newtype instance MVector s TAI64 = MV_TAI64 (MVector s (Word64,Word32,Word32))
newtype instance Vector    TAI64 = V_TAI64  (Vector    (Word64,Word32,Word32))

instance VM.MVector MVector TAI64 where
    {-# INLINE basicLength          #-}
    {-# INLINE basicUnsafeSlice     #-}
    {-# INLINE basicOverlaps        #-}
    {-# INLINE basicUnsafeNew       #-}
    {-# INLINE basicUnsafeReplicate #-}
    {-# INLINE basicUnsafeRead      #-}
    {-# INLINE basicUnsafeWrite     #-}
    {-# INLINE basicClear           #-}
    {-# INLINE basicSet             #-}
    {-# INLINE basicUnsafeCopy      #-}
    {-# INLINE basicUnsafeGrow      #-}
    basicLength (MV_TAI64 x)
        = VM.basicLength x
    basicUnsafeSlice i n (MV_TAI64 v)
        = MV_TAI64 $ VM.basicUnsafeSlice i n v
    basicOverlaps (MV_TAI64 v1) (MV_TAI64 v2)
        = VM.basicOverlaps v1 v2
    basicUnsafeNew n
        = MV_TAI64 `liftM` VM.basicUnsafeNew n
#if MIN_VERSION_vector(0,11,0)
    basicInitialize (MV_TAI64 v)
        = VM.basicInitialize v
    {-# INLINE basicInitialize      #-}
#endif
    basicUnsafeReplicate n (TAI64 s n' a)
        = MV_TAI64 `liftM` VM.basicUnsafeReplicate n (s,n',a)
    basicUnsafeRead (MV_TAI64 v) i
        = (\(s,n,a) -> TAI64 s n a) `liftM` VM.basicUnsafeRead v i
    basicUnsafeWrite (MV_TAI64 v) i (TAI64 s n a)
        = VM.basicUnsafeWrite v i (s,n,a)
    basicClear (MV_TAI64 v)
        = VM.basicClear v
    basicSet (MV_TAI64 v) (TAI64 s n a)
        = VM.basicSet v (s,n,a)
    basicUnsafeCopy (MV_TAI64 v1) (MV_TAI64 v2)
        = VM.basicUnsafeCopy v1 v2
    basicUnsafeMove (MV_TAI64 v1) (MV_TAI64 v2)
        = VM.basicUnsafeMove v1 v2
    basicUnsafeGrow (MV_TAI64 v) n
        = MV_TAI64 `liftM` VM.basicUnsafeGrow v n

instance VG.Vector Vector TAI64 where
    {-# INLINE basicUnsafeFreeze #-}
    {-# INLINE basicUnsafeThaw   #-}
    {-# INLINE basicLength       #-}
    {-# INLINE basicUnsafeSlice  #-}
    {-# INLINE basicUnsafeIndexM #-}
    {-# INLINE elemseq           #-}
    basicUnsafeFreeze (MV_TAI64 v)
        = V_TAI64 `liftM` VG.basicUnsafeFreeze v
    basicUnsafeThaw (V_TAI64 v)
        = MV_TAI64 `liftM` VG.basicUnsafeThaw v
    basicLength (V_TAI64 v)
        = VG.basicLength v
    basicUnsafeSlice i n (V_TAI64 v)
        = V_TAI64 $ VG.basicUnsafeSlice i n v
    basicUnsafeIndexM (V_TAI64 v) i
        = (\(s,n,a) -> TAI64 s n a) `liftM` VG.basicUnsafeIndexM v i
    basicUnsafeCopy (MV_TAI64 mv) (V_TAI64 v)
        = VG.basicUnsafeCopy mv v
    elemseq _ (TAI64 s n a) z
        = VG.elemseq (undefined :: Vector a) s
        $ VG.elemseq (undefined :: Vector a) n
        $ VG.elemseq (undefined :: Vector a) a z

instance Unbox TAI64


tai1970 :: AbsoluteTime
tai1970 = addAbsoluteTime (secondsToDiffTime 3506716800) taiEpoch

upp,piv :: Word64
upp = 2^(63 :: Int)
piv = 2^(62 :: Int)

toAbsoluteTime :: TAI64 -> AbsoluteTime
toAbsoluteTime (TAI64 secs nanos attos)
    | secs < upp = (secs' (secs - piv) + nanos' + attos')
                      `addAbsoluteTime` tai1970
    | secs < piv = (secs' (piv - secs) - nanos' - attos')
                      `addAbsoluteTime` tai1970
    | otherwise  = error "Outside universe lifetime"
  where
    secs' :: Word64 -> DiffTime
    secs' = secondsToDiffTime . fromIntegral

    nanos' = fromIntegral nanos * 10^^( -9 :: Int)
    attos' = fromIntegral attos * 10^^(-18 :: Int)

fromAbsoluteTime :: AbsoluteTime -> TAI64
fromAbsoluteTime abst
    | abst < tai1970 = mk (tai1970 `diffAbsoluteTime` abst)
    | otherwise      = mk (abst `diffAbsoluteTime` tai1970)
  where
    mk d = let (s,f) = properFraction d
               nanos = floor $ f * 10^( 9 :: Int)
               attos = floor $ f * 10^(18 :: Int)
            in TAI64 (s + piv) nanos attos

toUTCTime :: LeapSecondTable -> TAI64 -> UTCTime
toUTCTime lst = taiToUTCTime lst . toAbsoluteTime

fromUTCTime :: LeapSecondTable -> UTCTime -> TAI64
fromUTCTime lst = fromAbsoluteTime . utcToTAITime lst

toText :: TAI64Label -> Text
toText tl = TL.toStrict . TB.toLazyText $ case tl of
    TAI64S  tai -> TB.hexadecimal (taiSecs tai)
    TAI64N  tai -> mconcat
        [ TB.hexadecimal (taiSecs  tai)
        , TB.hexadecimal (taiNanos tai)
        ]
    TAI64NA tai -> mconcat
      [ TB.hexadecimal (taiSecs  tai)
      , TB.hexadecimal (taiNanos tai)
      , TB.hexadecimal (taiAttos tai)
      ]

fromText :: Text -> Either String TAI64
fromText = PT.parseOnly parseText

toByteString :: TAI64Label -> ByteString
toByteString tl = BL.toStrict . BB.toLazyByteString $ case tl of
    TAI64S  tai -> BB.word64Hex (taiSecs tai)
    TAI64N  tai -> mconcat
        [ BB.word64Hex (taiSecs  tai)
        , BB.word32Hex (taiNanos tai)
        ]
    TAI64NA tai -> mconcat
      [ BB.word64Hex (taiSecs  tai)
      , BB.word32Hex (taiNanos tai)
      , BB.word32Hex (taiAttos tai)
      ]

fromByteString :: ByteString -> Either String TAI64
fromByteString = PB.parseOnly parseByteString


class ParseInput a where
    _parseOnly   :: Parser a b -> a -> Either String b
    _take        :: Int -> Parser a a
    _hexadecimal :: (Integral x, Bits x) => Parser a x

instance ParseInput Text where
    _parseOnly   = PT.parseOnly
    _take        = PT.take
    _hexadecimal = PT.hexadecimal

instance ParseInput ByteString where
    _parseOnly   = PB.parseOnly
    _take        = PB.take
    _hexadecimal = PB.hexadecimal

parse :: ParseInput a => Parser a TAI64
parse = TAI64 <$> word64Hex <*> option 0 word32Hex <*> option 0 word32Hex
  where
    word64Hex = runParser _hexadecimal =<< _take 16
    word32Hex = runParser _hexadecimal =<< _take  8

    runParser p = either fail return . _parseOnly p

parseText :: Parser Text TAI64
parseText = parse

parseByteString :: Parser ByteString TAI64
parseByteString = parse
