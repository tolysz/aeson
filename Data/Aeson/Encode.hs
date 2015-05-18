{-# LANGUAGE BangPatterns, OverloadedStrings #-}

-- |
-- Module:      Data.Aeson.Encode
-- Copyright:   (c) 2012-2015 Bryan O'Sullivan
--              (c) 2011 MailRank, Inc.
-- License:     Apache
-- Maintainer:  Bryan O'Sullivan <bos@serpentine.com>
-- Stability:   experimental
-- Portability: portable
--
-- Efficiently serialize a JSON value.
--
-- Most frequently, you'll probably want to encode straight to UTF-8
-- (the standard JSON encoding) using 'encode'.
--
-- You can use the conversions to 'Builder's when embedding JSON messages as
-- parts of a protocol.
module Data.Aeson.Encode
    ( encode

    -- * Encoding to Builders
    , encodeToBuilder
    , encodeToTextBuilder

    -- * Deprecated
    , fromValue
    ) where

import Data.Aeson.Encode.Builder (encodeToBuilder)
import Data.Aeson.Types (ToJSON(..), Value(..), fromEncoding)
import Data.Monoid ((<>))
import Data.Scientific (FPFormat(..), Scientific, base10Exponent)
import Data.Text.Lazy.Builder
import Data.Text.Lazy.Builder.Scientific (formatScientificBuilder)
import Numeric (showHex)
import qualified Data.ByteString.Builder as B
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as H
import qualified Data.Text as T
import qualified Data.Vector as V

-- | Efficiently serialize a JSON value as a lazy 'L.ByteString'.
--
-- This is implemented in terms of the 'ToJSON' class's 'toEncoding' method.
encode :: ToJSON a => a -> L.ByteString
encode = B.toLazyByteString . fromEncoding . toEncoding
{-# INLINE encode #-}

removeMissing :: [(a,Value)] -> [(a,Value)]
removeMissing = filter (\(_,v) -> v /= Missing)

-- | Encode a JSON 'Value' to a "Data.Text" 'Builder', which can be
-- embedded efficiently in a text-based protocol.
--
-- If you are going to immediately encode straight to a
-- 'L.ByteString', it is more efficient to use 'encodeToBuilder'
-- instead.
encodeToTextBuilder :: Value -> Builder
encodeToTextBuilder =
    go
  where
    go Null       = {-# SCC "go/Null" #-} "null"
    go (Bool b)   = {-# SCC "go/Bool" #-} if b then "true" else "false"
    go (Number s) = {-# SCC "go/Number" #-} fromScientific s
    go (String s) = {-# SCC "go/String" #-} string s
    go (Array v)
        | V.null v = {-# SCC "go/Array" #-} "[]"
        | otherwise = {-# SCC "go/Array" #-}
                      singleton '[' <>
                      go (V.unsafeHead v) <>
                      V.foldr f (singleton ']') (V.unsafeTail v)
      where f a z = singleton ',' <> go a <> z
    go (Object m) = {-# SCC "go/Object" #-}
        case removeMissing (H.toList m) of
          (x:xs) -> singleton '{' <> one x <> foldr f (singleton '}') xs
          _      -> "{}"
      where f a z     = singleton ',' <> one a <> z
            one (k,v) = string k <> singleton ':' <> go v
    go Missing    = {-# SCC "go/Missing" #-} ""
        -- however should not happen unless ussed in array...
{-# DEPRECATED fromValue "Use 'encodeToTextBuilder' instead" #-}
fromValue :: Value -> Builder
fromValue = encodeToTextBuilder

string :: T.Text -> Builder
string s = {-# SCC "string" #-} singleton '"' <> quote s <> singleton '"'
  where
    quote q = case T.uncons t of
                Nothing      -> fromText h
                Just (!c,t') -> fromText h <> escape c <> quote t'
        where (h,t) = {-# SCC "break" #-} T.break isEscape q
    isEscape c = c == '\"' ||
                 c == '\\' ||
                 c < '\x20'
    escape '\"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"

    escape c
        | c < '\x20' = fromString $ "\\u" ++ replicate (4 - length h) '0' ++ h
        | otherwise  = singleton c
        where h = showHex (fromEnum c) ""

fromScientific :: Scientific -> Builder
fromScientific s = formatScientificBuilder format prec s
  where
    (format, prec)
      | base10Exponent s < 0 = (Generic, Nothing)
      | otherwise            = (Fixed,   Just 0)
