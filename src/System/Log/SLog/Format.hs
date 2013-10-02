{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-|
  This module defines the 'Format' datatype that describes how to format a log line.
  It also defines a template function @$(format _) :: 'Format'@ that allows C-style string formatting.
  The format string is thus parsed at compile time.

  The format string may contain the following elements:

  * %m - The logged message

  * %s - The severity of the message

  * %t - The name of the logging thread

  * %d(FORMAT) - A timestamp, formatted with FORMAT.

  The datetime FORMAT is a UNIX style format string detailed in "Data.Time.Format". The only difference is that closing brackets \')\'  inside the datetime format must be escaped with a backslash.

  Example:

  @
logFormat = $(format \"%d(%T) (%s) %t: %m\")
  @

  Which when logging with @'System.Log.SLog.logI' \"Something\"@ will produce something like:

  @
14:49:06 (INFO   ) main: Something
  @


  Example for escaping \')\' in the datetime format string:

  @
logFormat = $(format \"%d((%F\\\\)(%T\\\\)) %m\")
  @

  Which when logging with @'System.Log.SLog.logI' \"Something\"@ will produce:

  @
(2013-10-02)(16:26:21) Something
  @

  Note how we need an additional \'\\' because of Haskell strings
-}
module System.Log.SLog.Format
    (
      -- *Format
      FormatElem(..)
    , Format
    , format )
where

import Control.Applicative
import Language.Haskell.TH
import Language.Haskell.TH.Lift
import qualified Data.Text as T
import qualified Data.Attoparsec.Text as A
import Data.Time.Format
import Data.Time.LocalTime
import System.Locale
import Data.String

-- An inefficient orphan Lift instance for Text. It is run at compile time so no worries
instance Lift T.Text where
    lift t = let s = T.unpack t in [| fromString s :: T.Text |]

-- | A 'Lift'able formatting element
data FormatElemTH
    = MessageElemTH
    | SeverityElemTH
    | StringElemTH T.Text
    | DateTimeElemTH String
    | ThreadElemTH
      deriving (Show)

type FormatTH = [FormatElemTH]

-- 'FormatElem' is the same as FormatElemTH, onlt this time it stores the datetime formatting closure instead of the format string
-- | A 'FormatElem' is a formatting element
data FormatElem
    = MessageElem
    | SeverityElem
    | StringElem T.Text
    | DateTimeElem (ZonedTime -> T.Text)
    | ThreadElem

-- | 'Format' is the type of a full format. It is simply a list of 'FormatElem's
type Format = [FormatElem]

-- | finaliseFormat finalises a format that represents datetime elements as a UNIX datetime format string and turns them into a formatting closure. This method also concatenates adjacent 'StringElem's
finaliseFormat :: FormatTH -> Format
finaliseFormat (StringElemTH s0 : StringElemTH s1 : rest) = finaliseFormat $ StringElemTH (T.append s0 s1) : rest
finaliseFormat fs = map for' fs
  where
    for' MessageElemTH = MessageElem
    for' SeverityElemTH = SeverityElem
    for' (StringElemTH s) = StringElem s
    for' (DateTimeElemTH s) = DateTimeElem $ T.pack . formatTime defaultTimeLocale s
    for' ThreadElemTH = ThreadElem


-- | formatParser parses a format string into FormatElemTHs
formatParser :: A.Parser [ExpQ] -- FormatTH
formatParser
    = A.endOfInput *> return [] <|> do
        e <- A.choice [ A.takeWhile1 (/= '%') >>= \t -> return [|StringElemTH t|]
                      , A.char '%' *> elemParser
                      ]
        (e :) <$> formatParser

-- | elemParser parses a single FormatElemTH
elemParser :: A.Parser ExpQ -- FormatElemTH
elemParser = A.choice [ A.char '%' *> return [|StringElemTH "%"|]
                      , A.char 'm' *> return [|MessageElemTH|]
                      , A.char 's' *> return [|SeverityElemTH|]
                      , A.char 'd' *> datetimeParser >>= \f -> return [|DateTimeElemTH f|]
                      , A.char 't' *> return [|ThreadElemTH|]
                      ]

-- | datetimeParser parses a datetime element
datetimeParser :: A.Parser String
datetimeParser = fmap T.unpack $ A.char '(' *> datetimeParser' <* A.char ')'
  where
    datetimeParser' :: A.Parser T.Text
    datetimeParser' = do
      s <- A.takeWhile (\c -> c /= '\\' && c /= ')')
      A.choice [ A.string "\\)" *> ((\r -> T.concat [s, ")", r]) <$> datetimeParser')
               , liftA3
                   (\a b c -> a `T.cons` b `T.cons` c)
                   (A.char '\\')
                   A.anyChar
                   datetimeParser'
               , return s
               ]


-- | @$('format' _) :: 'Format'@ is a template function that parses the passed in format string, then finalises it and returns a 'Format'. 
format :: String -> ExpQ
format s = [| finaliseFormat $(mat s) |]

-- | $('mat' _) :: FormatTH is a template function that parses the passed in format string and returns a FormatTH.
mat :: String -> ExpQ
mat s = case A.parseOnly formatParser $ T.pack s of
          Left err -> fail err
          Right exps -> ListE <$> sequence exps
