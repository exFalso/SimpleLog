{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
{-
  This module defines the LogFormat datatype that describes how to format a log line.
  It also defines a template function $(format _) :: LogFormat that allows C-style formatting.
  The format string is thus parsed at compile time
-}
module System.Log.SLog.Format
    ( LogFormatElem(..)
    , LogFormat
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

data LogFormatElemTH
    = MessageElemTH
    | SeverityElemTH
    | StringElemTH T.Text
    | DateTimeElemTH String -- (ZonedTime -> String)
    | ThreadElemTH
      deriving (Show)

type LogFormatTH = [LogFormatElemTH]

-- stores the datetime formatting closure instead of format string
data LogFormatElem
    = MessageElem
    | SeverityElem
    | StringElem T.Text
    | DateTimeElem (ZonedTime -> T.Text)
    | ThreadElem

type LogFormat = [LogFormatElem]

finaliseFormat :: LogFormatTH -> LogFormat
finaliseFormat = map for'
  where
    for' MessageElemTH = MessageElem
    for' SeverityElemTH = SeverityElem
    for' (StringElemTH s) = StringElem s
    for' (DateTimeElemTH s) = DateTimeElem $ T.pack . formatTime defaultTimeLocale s
    for' ThreadElemTH = ThreadElem

formatParser :: A.Parser [(ExpQ)] -- LogFormat
formatParser
    = A.endOfInput *> return [] <|> do
        e <- A.choice [ A.takeWhile1 (/= '%') >>= \t -> return [|StringElemTH t|]
                      , A.char '%' *> elemParser
                      ]
        (e :) <$> formatParser

elemParser :: A.Parser (ExpQ) -- LogFormatElemTH
elemParser = A.choice [ A.char '%' *> return [|StringElemTH "%"|]
                      , A.char 'm' *> return [|MessageElemTH|]
                      , A.char 's' *> return [|SeverityElemTH|]
                      , A.char 'd' *> datetimeParser >>= \f -> return [|DateTimeElemTH f|]
                      , A.char 'n' *> return [|ThreadElemTH|]
                      ]

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


-- $(mat _) :: LogFormatTH
mat :: String -> ExpQ
mat s = case A.parseOnly formatParser $ T.pack s of
          Left err -> fail err
          Right exps -> ListE <$> sequence exps

-- $(format _) :: LogFormat
format :: String -> ExpQ
format s = [| finaliseFormat $(mat s) |]
