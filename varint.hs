{-# LANGUAGE BangPatterns, ForeignFunctionInterface #-}

module Main (main, decodeVarInt, cVarInt, varInt) where

import Data.Bits
import qualified Data.ByteString as B
import qualified Data.ByteString.Unsafe as B
import qualified Data.ByteString.Internal as B

import Data.Int

import System.IO.Unsafe

import Foreign.C
import Foreign

import Criterion.Main
import Data.Int

import Control.DeepSeq

main = do
	print (decodeVarInt (B.pack [0x01]))
	print (decodeVarInt (B.pack [0x80, 0x01]))
	--print (decodeVarInt2 s2)
	print (cVarInt (B.pack [0x01]))
	print (cVarInt (B.pack [0x80, 0x01]))
	print (cVarInt2 s2)
	defaultMain [
	  bench "haskell" $ nf decodeVarInt s,
	  --bench "haskell2" $ nf decodeVarInt2 s2,
	  bench "c" $ nf cVarInt s,
	  bench "c2" $ nf cVarInt2 s2,
	  bench "protoReader" $ nf protoRepeat s
	  ]

s = B.pack [0x80, 0xa0, 0x11]
s2 = B.concat [s, s]
{-
decodeVarInt2 :: B.ByteString ->  (Int32, Int32)
decodeVarInt2 str = do
	i <- decodeVarIntReal str
	i2 <- decodeVarIntReal $! B.unsafeDrop used str
	return $ (i, i2)
-}
data Decode a = NotEnoughInput
            | InvalidData
            | HereItIs {-# UNPACK #-} !Int !a
            deriving Show

{-
instance Monad Decode where
	return a = HereItIs 0 a
	a >>= b = case a of
		        NotEnoughInput -> NotEnoughInput
		        InvalidData -> InvalidData
		        HereItIs used value -> case b value of
		        	                     NotEnoughInput -> NotEnoughInput
		        	                     InvalidData -> InvalidData
		        	                     HereItIs used' value' -> HereItIs (used+used') value'
-}
instance Control.DeepSeq.NFData (Decode a) where
	rnf NotEnoughInput = ()
	rnf InvalidData = ()
	rnf (HereItIs _ _) = ()

{-# SPECIALIZE varInt :: B.ByteString -> Decode Int8 #-}
{-# SPECIALIZE varInt :: B.ByteString -> Decode Int16 #-}
{-# SPECIALIZE varInt :: B.ByteString -> Decode Int32 #-}
{-# SPECIALIZE varInt :: B.ByteString -> Decode Int64 #-}
varInt :: (Num a, Bits a) => B.ByteString -> Decode a
varInt str = go acc0 0 0 (B.take bytes str)
  where
  	!acc0 = 0
  	!bytes = ceiling (fromIntegral (bitSize acc0) / 7)
  	go !acc !pos !i !str
	  | B.length str <= i = if i == bytes
	  	                      then InvalidData
	                          else NotEnoughInput
	  | otherwise = 
	  	  let !byte = B.unsafeIndex str i
	  	      !continue = byte >= 128
	  	      !val = fromIntegral (byte .&. 0x7F)
	  	      !acc' = (val `unsafeShiftL` pos) .|. acc
	  	      !i' = i+1
	  	      !pos' = pos + 7
	  	  in if continue
	  	  	   then go acc' pos' i' str
	  	  	   else HereItIs i' acc'

decodeVarIntReal :: B.ByteString -> Maybe (Int32, Int)
decodeVarIntReal = goH 0 0 0 . B.take 5
  where
  	goH !acc !pos !i !str
	  | B.length str <= i = if i == 5 
	  	                      then error "asking for too much"
	                          else Nothing
	  | otherwise = 
	  	  let !byte = B.unsafeIndex str i
	  	      !continue = byte >= 128
	  	      !val = fromIntegral (byte .&. 0x7F)
	  	      !acc' = (val `unsafeShiftL` pos) .|. acc
	  	      !i' = i+1
	  	      !pos' = pos + 7
	  	  in if continue
	  	  	   then goH acc' pos' i' str
	  	  	   else Just (acc', i')

data SMaybe = Nutting
            | Justa {-# UNPACK #-} !Int32 {-# UNPACK #-} !Int

decodeVarInt :: B.ByteString -> Decode Int64
decodeVarInt = go 1000000 NotEnoughInput
  where
  	go :: Int -> Decode Int64 -> B.ByteString -> Decode Int64
  	go 0 !v !_  = v
  	go n _ str = go (n-1) (varInt str) str

cVarInt :: B.ByteString -> Maybe (Int32, Int)
cVarInt str = unsafeDupablePerformIO $ B.unsafeUseAsCStringLen str $ \(strPtr, len) ->
  alloca $ \resultPtr -> do
    !status <- decodeVarInt_c strPtr (fromIntegral len) resultPtr
    if status /= 0
      then do val <- peek resultPtr
              return $! Just $! (fromIntegral val, fromIntegral status)
      else return Nothing

cVarInt2 :: B.ByteString -> Maybe (Int32, Int32, Int)
cVarInt2 str = unsafeDupablePerformIO $ B.unsafeUseAsCStringLen str $ \(strPtr, len) ->
  alloca $ \result1Ptr ->
  alloca $ \result2Ptr -> do
    !status <- decodeVarInt_c2 strPtr (fromIntegral len) result1Ptr result2Ptr
    -- print status
    if status /= 0
      then do val1 <- peek result1Ptr
              val2 <- peek result2Ptr
              return $! Just $! (fromIntegral val1, fromIntegral val2, fromIntegral status)
      else return Nothing


foreign import ccall unsafe "decodeVarInt" decodeVarInt_c :: Ptr CChar -> CInt -> Ptr CUInt -> IO CUInt

foreign import ccall unsafe "decodeVarInt2" decodeVarInt_c2 :: Ptr CChar -> CInt -> Ptr CUInt -> Ptr CUInt -> IO CUInt




protoRepeat :: B.ByteString -> Maybe (Int32, Int)
protoRepeat = go 1000000 Nothing
  where
  	go :: Int -> Maybe (Int32, Int) -> B.ByteString -> Maybe (Int32, Int)
  	go 0 !v !_  = v
  	go n _ str = go (n-1) (protoReader str) str


data TU s = TU'OK !s !Int


protoReader :: B.ByteString -> Maybe (Int32, Int)
protoReader ss@(B.PS fp off len) =
      let (TU'OK x i) = 
            System.IO.Unsafe.unsafePerformIO $ withForeignPtr fp $ \ptr0 -> do
                if ptr0 == nullPtr || len < 1 then error "Get.decode7unrolled: ByteString invariant failed" else do
                let ok :: s -> Int -> IO (TU s)
                    ok x0 i0 = return (TU'OK x0 i0)
                    more,err :: IO (TU Int32)
                    more = return (TU'OK 0 0)  -- decode7
                    err = return (TU'OK 0 (-1))  -- throwError
                    {-# INLINE ok #-}
                    {-# INLINE more #-}
                    {-# INLINE err #-}

--                -- Next line is segfault fix for null bytestrings from Nathan Howell <nhowell@alphaheavy.com>
--                if ptr0 == nullPtr then more else do

                let start = ptr0 `plusPtr` off :: Ptr Word8
                b'1 <- peek start
                if b'1 < 128 then ok (fromIntegral b'1) 1 else do
                let !val'1 = fromIntegral (b'1 .&. 0x7F)
                    !end = start `plusPtr` len
                    !ptr2 = start `plusPtr` 1 :: Ptr Word8
                if ptr2 >= end then more else do

                b'2 <- peek ptr2
                if b'2 < 128 then ok (val'1 .|. (fromIntegral b'2 `shiftL` 7)) 2 else do
                let !val'2 = (val'1 .|. (fromIntegral (b'2 .&. 0x7F) `shiftL` 7))
                    !ptr3 = ptr2 `plusPtr` 1
                if ptr3 >= end then more else do

                b'3 <- peek ptr3
                if b'3 < 128 then ok (val'2 .|. (fromIntegral b'3 `shiftL` 14)) 3 else do
                let !val'3 = (val'2 .|. (fromIntegral (b'3 .&. 0x7F) `shiftL` 14))
                    !ptr4 = ptr3 `plusPtr` 1
                if ptr4 >= end then more else do

                b'4 <- peek ptr4
                if b'4 < 128 then ok (val'3 .|. (fromIntegral b'4 `shiftL` 21)) 4 else do
                let !val'4 = (val'3 .|. (fromIntegral (b'4 .&. 0x7F) `shiftL` 21))
                    !ptr5 = ptr4 `plusPtr` 1
                if ptr5 >= end then more else do

                b'5 <- peek ptr5
                if b'5 < 128 then ok (val'4 .|. (fromIntegral b'5 `shiftL` 28)) 5 else do
                let !val'5 = (val'4 .|. (fromIntegral (b'5 .&. 0x7F) `shiftL` 28))
                    !ptr6 = ptr5 `plusPtr` 1
                if ptr6 >= end then more else do
                   
                b'6 <- peek ptr6
                if b'6 < 128 then ok (val'5 .|. (fromIntegral b'6 `shiftL` 35)) 6 else do
                let !val'6 = (val'5 .|. (fromIntegral (b'6 .&. 0x7F) `shiftL` 35))
                    !ptr7 = ptr6 `plusPtr` 1
                if ptr7 >= end then more else do
                   
                b'7 <- peek ptr7
                if b'7 < 128 then ok (val'6 .|. (fromIntegral b'7 `shiftL` 42)) 7 else do
                let !val'7 = (val'6 .|. (fromIntegral (b'7 .&. 0x7F) `shiftL` 42))
                    !ptr8 = ptr7 `plusPtr` 1
                if ptr8 >= end then more else do
                   
                b'8 <- peek ptr8
                if b'8 < 128 then ok (val'7 .|. (fromIntegral b'8 `shiftL` 49)) 8 else do
                let !val'8 = (val'7 .|. (fromIntegral (b'8 .&. 0x7F) `shiftL` 49))
                    !ptr9 = ptr8 `plusPtr` 1
                if ptr9 >= end then more else do
                   
                b'9 <- peek ptr9
                if b'9 < 128 then ok (val'8 .|. (fromIntegral b'9 `shiftL` 56)) 9 else do
                let !val'9 = (val'8 .|. (fromIntegral (b'9 .&. 0x7F) `shiftL` 56))
                    !ptrA = ptr9 `plusPtr` 1
                if ptrA >= end then more else do

                b'A <- peek ptrA
                if b'A < 128 then ok (val'9 .|. (fromIntegral b'A `shiftL` 63)) 10 else do
                err

      in if i > 0
           then let ss' = (B.unsafeDrop i ss)
                    n' = fromIntegral i
                in Just (x, n')
           else if i==0 then Nothing
                        else Nothing