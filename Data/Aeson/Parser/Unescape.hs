{-# LANGUAGE ForeignFunctionInterface #-}
{-# LANGUAGE MagicHash                #-}
{-# LANGUAGE UnliftedFFITypes         #-}

module Data.Aeson.Parser.Unescape (
  unescapeText
) where

import           Control.Exception          (evaluate, throw, try)
import           Control.Monad              (when)
import           Data.ByteString            as B
import           Data.Bits                  (shiftL, (.|.))
import           Data.Text                  (Text)
import qualified Data.Text.Array            as A
import           Data.Text.Encoding.Error   (UnicodeException (..))
import           Data.Text.Internal.Private (runText)
import           Data.Text.Unsafe           (unsafeDupablePerformIO)
import           Data.Word                  (Word8, Word16, Word32)

-- Different UTF states.
data Utf = 
    UtfGround
  | UtfTail1
  | UtfU32e0
  | UtfTail2
  | UtfU32ed
  | Utf843f0
  | Tail3
  | Utf843f4
  deriving (Eq)

data State = 
    StateNone
  | StateBackslash
  | StateUtf !Word32 !Utf
  | StateU0
  | StateU1 !Word16
  | StateU2 !Word16
  | StateU3 !Word16
  deriving (Eq)

-- decode word  = ...

decodeHex :: Word8 -> Word16
decodeHex 48  = 0  -- '0' 
decodeHex 49  = 1  -- '1' 
decodeHex 50  = 2  -- '2' 
decodeHex 51  = 3  -- '3' 
decodeHex 52  = 4  -- '4' 
decodeHex 53  = 5  -- '5' 
decodeHex 54  = 6  -- '6' 
decodeHex 55  = 7  -- '7' 
decodeHex 56  = 8  -- '8' 
decodeHex 57  = 9  -- '9' 
decodeHex 65  = 10 -- 'A' 
decodeHex 97  = 10 -- 'a' 
decodeHex 66  = 11 -- 'B' 
decodeHex 98  = 11 -- 'b' 
decodeHex 67  = 12 -- 'C' 
decodeHex 99  = 12 -- 'c' 
decodeHex 68  = 13 -- 'D' 
decodeHex 100 = 13 -- 'd' 
decodeHex 69  = 14 -- 'E' 
decodeHex 101 = 14 -- 'e' 
decodeHex 70  = 15 -- 'F' 
decodeHex 102 = 15 -- 'f' 
decodeHex _ = throwDecodeError

unescapeText' :: ByteString -> Text
unescapeText' bs = runText $ \done -> do
    dest <- A.new len
    (pos, finalState) <- B.foldl' (f' dest) (return (0, StateNone)) bs

    -- Check final state.
    when ( finalState /= StateNone)
      throwDecodeError

    done dest undefined -- TODO: pos, pos-1??? XXX

    where
      len = B.length bs

      f' dest m c = m >>= \s -> f dest s c

      -- No pending state.
      f _ (pos, StateNone) c = undefined
				-- TODO: c is a Word8, need to build a Word32??? XXX

      -- In the middle of parsing a UTF string.
      f _ (pos, StateUtf point st) c = undefined

      -- In the middle of escaping a backslash.
      f dest (pos, StateBackslash)  34 = writeAndReturn dest pos 34 StateNone -- "
      f dest (pos, StateBackslash)  92 = writeAndReturn dest pos 92 StateNone -- \
      f dest (pos, StateBackslash)  47 = writeAndReturn dest pos 47 StateNone -- /
      f dest (pos, StateBackslash)  98 = writeAndReturn dest pos  8 StateNone -- b
      f dest (pos, StateBackslash) 102 = writeAndReturn dest pos 12 StateNone -- f
      f dest (pos, StateBackslash) 110 = writeAndReturn dest pos 10 StateNone -- n
      f dest (pos, StateBackslash) 114 = writeAndReturn dest pos 13 StateNone -- r
      f dest (pos, StateBackslash) 116 = writeAndReturn dest pos  9 StateNone -- t
      f dest (pos, StateBackslash) 117 = return (pos, StateU0)                -- u
      f dest (pos, StateBackslash) _   = throwDecodeError

      -- Processing '\u'.
      f _ (pos, StateU0) c = 
        let w = decodeHex c in
        return (pos, StateU1 (w `shiftL` 12))

      f _ (pos, StateU1 w') c = 
        let w = decodeHex c in
        return (pos, StateU1 (w' .|. (w `shiftL` 8)))

      f _ (pos, StateU2 w') c = 
        let w = decodeHex c in
        return (pos, StateU1 (w' .|. (w `shiftL` 4)))

      f _ (pos, StateU3 w') c = 
        let w = decodeHex c in
				let v = w' .|. w in
				undefined -- TODO: Check for surrogates... XXX
				writeAndReturn 

{-# INLINE unescapeText' #-}

-- Is this a Word16???
writeAndReturn dest pos char res = do
  undefined
  -- writeWord16Array# dest pos char
  -- return (pos + 1, res)
      
throwDecodeError = 
  let desc = "Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream" in
  throw (DecodeError desc Nothing)




{-
import           Control.Exception          (evaluate, throw, try)
import           Control.Monad.ST.Unsafe    (unsafeIOToST, unsafeSTToIO)
import           Data.ByteString            as B
import           Data.ByteString.Internal   as B hiding (c2w)
import qualified Data.Text.Array            as A
import           Data.Text.Encoding.Error   (UnicodeException (..))
import           Data.Text.Internal         (Text (..))
import           Data.Text.Internal.Private (runText)
import           Data.Text.Unsafe           (unsafeDupablePerformIO)
import           Data.Word                  (Word8)
import           Foreign.C.Types            (CInt (..), CSize (..))
import           Foreign.ForeignPtr         (withForeignPtr)
import           Foreign.Marshal.Utils      (with)
import           Foreign.Ptr                (Ptr, plusPtr)
import           Foreign.Storable           (peek)
import           GHC.Base                   (MutableByteArray#)

foreign import ccall unsafe "_js_decode_string" c_js_decode
    :: MutableByteArray# s -> Ptr CSize
    -> Ptr Word8 -> Ptr Word8 -> IO CInt

unescapeText' :: ByteString -> Text
unescapeText' (PS fp off len) = runText $ \done -> do
  let go dest = withForeignPtr fp $ \ptr ->
        with (0::CSize) $ \destOffPtr -> do
          let end = ptr `plusPtr` (off + len)
              loop curPtr = do
                res <- c_js_decode (A.maBA dest) destOffPtr curPtr end
                case res of
                  0 -> do
                    n <- peek destOffPtr
                    unsafeSTToIO (done dest (fromIntegral n))
                  _ ->
                    throw (DecodeError desc Nothing)
          loop (ptr `plusPtr` off)
  (unsafeIOToST . go) =<< A.new len
 where
  desc = "Data.Text.Internal.Encoding.decodeUtf8: Invalid UTF-8 stream"
{-# INLINE unescapeText' #-}
-}

unescapeText :: ByteString -> Either UnicodeException Text
unescapeText = unsafeDupablePerformIO . try . evaluate . unescapeText'
{-# INLINE unescapeText #-}
