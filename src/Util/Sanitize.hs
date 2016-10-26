-- taken from https://github.com/jaspervdj/blaze-html/blob/2c4513e30ce768517b8d7b7b154d438f55217006/src/Util/Sanitize.hs

-- Copyright Jasper Van der Jeugt 2010

-- All rights reserved.

-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are met:

--     * Redistributions of source code must retain the above copyright
--       notice, this list of conditions and the following disclaimer.

--     * Redistributions in binary form must reproduce the above
--       copyright notice, this list of conditions and the following
--       disclaimer in the documentation and/or other materials provided
--       with the distribution.

--     * Neither the name of Jasper Van der Jeugt nor the names of other
--       contributors may be used to endorse or promote products derived
--       from this software without specific prior written permission.

-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
-- "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
-- LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
-- A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
-- OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
-- SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
-- LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
-- DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
-- THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
-- (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
-- OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
-- | A program to sanitize an HTML tag to a Haskell function.
--
module Util.Sanitize
    ( sanitize
    , keywords
    , prelude
    ) where

import Data.Char (toLower, toUpper)
import Data.Set (Set)
import qualified Data.Set as S

-- | Sanitize a tag. This function returns a name that can be used as
-- combinator in haskell source code.
--
-- Examples:
--
-- > sanitize "class" == "class_"
-- > sanitize "http-equiv" == "httpEquiv"
--
sanitize :: String -> String
sanitize str
    | lower  == "doctypehtml" = "docTypeHtml"
    | otherwise               = appendUnderscore $ removeDash lower
  where
    lower = map toLower str

    -- Remove a dash, replacing it by camelcase notation
    --
    -- Example:
    --
    -- > removeDash "foo-bar" == "fooBar"
    --
    removeDash ('-' : x : xs) = toUpper x : removeDash xs
    removeDash (x : xs) = x : removeDash xs
    removeDash [] = []

    appendUnderscore t | t `S.member` keywords = t ++ "_"
                       | otherwise             = t

-- | A set of standard Haskell keywords, which cannot be used as combinators.
--
keywords :: Set String
keywords = S.fromList
    [ "case", "class", "data", "default", "deriving", "do", "else", "if"
    , "import", "in", "infix", "infixl", "infixr", "instance" , "let", "module"
    , "newtype", "of", "then", "type", "where"
    ]

-- | Set of functions from the Prelude, which we do not use as combinators.
--
prelude :: Set String
prelude = S.fromList
    [ "abs", "acos", "acosh", "all", "and", "any", "appendFile", "asTypeOf"
    , "asin", "asinh", "atan", "atan2", "atanh", "break", "catch", "ceiling"
    , "compare", "concat", "concatMap", "const", "cos", "cosh", "curry", "cycle"
    , "decodeFloat", "div", "divMod", "drop", "dropWhile", "either", "elem"
    , "encodeFloat", "enumFrom", "enumFromThen", "enumFromThenTo", "enumFromTo"
    , "error", "even", "exp", "exponent", "fail", "filter", "flip"
    , "floatDigits", "floatRadix", "floatRange", "floor", "fmap", "foldl"
    , "foldl1", "foldr", "foldr1", "fromEnum", "fromInteger", "fromIntegral"
    , "fromRational", "fst", "gcd", "getChar", "getContents", "getLine", "head"
    , "id", "init", "interact", "ioError", "isDenormalized", "isIEEE"
    , "isInfinite", "isNaN", "isNegativeZero", "iterate", "last", "lcm"
    , "length", "lex", "lines", "log", "logBase", "lookup", "map", "mapM"
    , "mapM_", "max", "maxBound", "maximum", "maybe", "min", "minBound"
    , "minimum", "mod", "negate", "not", "notElem", "null", "odd", "or"
    , "otherwise", "pi", "pred", "print", "product", "properFraction", "putChar"
    , "putStr", "putStrLn", "quot", "quotRem", "read", "readFile", "readIO"
    , "readList", "readLn", "readParen", "reads", "readsPrec", "realToFrac"
    , "recip", "rem", "repeat", "replicate", "return", "reverse", "round"
    , "scaleFloat", "scanl", "scanl1", "scanr", "scanr1", "seq", "sequence"
    , "sequence_", "show", "showChar", "showList", "showParen", "showString"
    , "shows", "showsPrec", "significand", "signum", "sin", "sinh", "snd"
    , "span", "splitAt", "sqrt", "subtract", "succ", "sum", "tail", "take"
    , "takeWhile", "tan", "tanh", "toEnum", "toInteger", "toRational"
    , "truncate", "uncurry", "undefined", "unlines", "until", "unwords", "unzip"
    , "unzip3", "userError", "words", "writeFile", "zip", "zip3", "zipWith"
    , "zipWith3"
    ]
