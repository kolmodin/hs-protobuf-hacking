{-# LANGUAGE BangPatterns, TypeOperators, ExistentialQuantification #-}
module Main where

import Data.Int
import Data.Word
import Data.Bits

import qualified Data.ByteString as B
import qualified Data.ByteString.Internal as B
import qualified Data.ByteString.Unsafe as B

import Foreign.ForeignPtr (withForeignPtr)
import Foreign.Ptr (Ptr(..), plusPtr, minusPtr)
import Foreign.Storable

import qualified Data.Text as T
import qualified Data.Text.Encoding as T

import Criterion.Main

type FieldNumber = Word64
type BitVector = Word64

data a :*: b = !a :*: !b

data WireValue
  = VarInt !FieldNumber !Int64
  | LengthDelimited !FieldNumber !B.ByteString

data Result a
  = Done !a
  | Fail String
  deriving Show

class Buildable a where
  newBuilder :: IO (Builder a)

data Builder a
  = forall s. Builder BitVector s (WireValue -> BitVector -> s -> IO (s :*: BitVector))
                                  (s -> IO a)

{-
message Test1 {
  required int32 a = 1;
}
-}

data MessageTest1 = MessageTest1 { test1a :: !Int32 } deriving Show
mkTest1Builder = return $ Builder 1 (MessageTest1 0) updateTest1 (return.id)
updateTest1 !wv !bv !value
  = case wv of
      VarInt 1 int64 -> return $ value { test1a = fromIntegral int64 } :*: clearBit bv 0
      _ -> return $ value :*: bv
instance Buildable MessageTest1 where
  newBuilder = mkTest1Builder

{-
message Test2 {
  required string b = 2;
}
-}
data MessageTest2 = MessageTest2 { test2b :: !T.Text } deriving Show
mkTest2Builder = return $ Builder 1 (MessageTest2 T.empty) updateTest2 (return.id)
updateTest2 !wv !bv !value
  = case wv of
      LengthDelimited 2 bytes -> return $ value { test2b = T.decodeUtf8 bytes } :*: clearBit bv 0
      _ -> return $ value :*: bv
instance Buildable MessageTest2 where
  newBuilder = mkTest2Builder


{-
message Test3 {
  required Test1 c = 3;
}
-}
data MessageTest3 = MessageTest3 { test3c :: MessageTest1 } deriving Show
mkTest3Builder = return $ Builder 1 (MessageTest3 undefined) updateTest3 (return.id)
updateTest3 !wv !bv !value
  = case wv of
      LengthDelimited 3 bytes -> return $ value { test3c = case build bytes of Done x -> x } :*: clearBit bv 0
      _ -> return $ value :*: bv
instance Buildable MessageTest3 where
  newBuilder = mkTest3Builder

update :: Builder a -> WireValue -> IO (Builder a)
update builder@(Builder bv value updateFn finalizeFn) vw = do
  x <- updateFn vw bv value
  case x of
    value' :*: bv' -> return $ Builder bv' value' updateFn finalizeFn

finalize :: Builder a -> IO (Result a)
finalize (Builder 0 value _ finalizeFn) = fmap Done (finalizeFn value)
finalize _ = return $ Fail "all required values not set"

data WireType = Varint | Lengthdelimited deriving Show
wireType :: Int64 -> WireType
wireType t =
  case t .&. 7 of
    0 -> Varint
    2 -> Lengthdelimited
    n -> error ("unknown type: " ++ show n ++ " from tag: " ++ show t)

mkFieldNumber :: Int64 -> Word64
mkFieldNumber key = (fromIntegral key) `shiftR` 3

build :: Buildable a => B.ByteString -> Result a
build (B.PS fp offset length) = B.inlinePerformIO $ withForeignPtr fp $ \fpPtr -> do
  let !ptr0 = fpPtr `plusPtr` offset
  let !endPtr = ptr0 `plusPtr` length
  let mkBS ptr len = B.PS fp (ptr `minusPtr` fpPtr) len
  let go ptr builder@(Builder bv v updateFn finalizeFn) = go' ptr bv v
        where
          go' ptr bv v
            | ptr >= endPtr = finalize (Builder bv v updateFn finalizeFn)
            | ptr < endPtr = do
                varInt ptr endPtr $ \ptr key -> do
                let !fieldNumber = mkFieldNumber key
                case wireType key of
                  Varint -> do
                    varInt ptr endPtr $ \ptr value -> do
                      let !wv = VarInt fieldNumber value
                      v' :*: bv' <- updateFn wv bv v
                      go' ptr bv' v'
                  Lengthdelimited -> do
                    varInt ptr endPtr $ \ptr len -> do
                      let len' = fromIntegral len
                          !wv = LengthDelimited fieldNumber $! (mkBS ptr len')
                      v' :*: bv' <- updateFn wv bv v
                      go' (ptr `plusPtr` len') bv' v'
  go ptr0 =<< newBuilder

{-# INLINE varInt #-}
varInt :: Ptr Word8 -> Ptr Word8 -> (Ptr Word8 -> Int64 -> IO (Result b)) -> IO (Result b)
varInt ptr0 endPtr0 k = go acc0 ptr0 0 
  where
        !acc0 = 0
        !bytes = ceiling (fromIntegral (bitSize acc0) / 7)
        !endPtr = min endPtr0 (ptr0 `plusPtr` bytes) 
        go !acc !ptr !pos
          | ptr >= endPtr = return $ Fail "invalid varint"
          | otherwise = do
              !byte <- peek ptr
              let !continue = byte >= 128
                  !val = fromIntegral (byte .&. 0x7F)
                  !acc' = (val `unsafeShiftL` pos) .|. acc
                  !ptr' = ptr `plusPtr` 1
                  !pos' = pos + 7
              if continue
                   then go acc' ptr' pos'
                   else k ptr' acc'

main :: IO ()
main = do
  let msg1 = B.pack [0x08, 0x96, 0x01]
      msg2 = B.pack [0x12, 0x07, 0x74, 0x65, 0x73, 0x74, 0x69, 0x6e, 0x67]
      msg3 = B.pack [0x1a, 0x03, 0x08, 0x96, 0x01]
  print (build msg1 :: Result MessageTest1)
  print (build msg2 :: Result MessageTest2)
  print (build msg3 :: Result MessageTest3)
  defaultMain
    [ bench "MessageTest1" (whnf (build :: B.ByteString -> Result MessageTest1) msg1)
    , bench "MessageTest2" (whnf (build :: B.ByteString -> Result MessageTest2) msg2)
    , bench "MessageTest3" (whnf (build :: B.ByteString -> Result MessageTest3) msg3) ]

