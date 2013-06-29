{-# LANGUAGE OverloadedStrings, TemplateHaskell #-}
module Format where

import Control.Applicative
import Language.Haskell.TH
import qualified Data.ByteString.Char8 as BS
import qualified Data.Attoparsec.Char8 as A
import Data.Time.Format
import Data.Time.LocalTime
import System.Locale

data LogFormatElemTH
    = MessageElemTH
    | SeverityElemTH
    | StringElemTH String
    | DateTimeElemTH String -- (ZonedTime -> String)
    | ThreadElemTH
      deriving (Show)
-- deriveLift ''LogFormatElemTH

type LogFormatTH = [LogFormatElemTH]

-- stores the datetime formatting closure instead of format string
data LogFormatElem
    = MessageElem
    | SeverityElem
    | StringElem String
    | DateTimeElem (ZonedTime -> String)
    | ThreadElem

type LogFormat = [LogFormatElem]

for :: LogFormatTH -> LogFormat
for = map for'
    where
      for' MessageElemTH = MessageElem
      for' SeverityElemTH = SeverityElem
      for' (StringElemTH s) = StringElem s
      for' (DateTimeElemTH s) = DateTimeElem $ formatTime defaultTimeLocale s
      for' ThreadElemTH = ThreadElem

formatParser :: A.Parser [(Q Exp)] -- LogFormat
formatParser
    = A.endOfInput *> return [] <|> do
        e <- A.choice [ A.takeWhile1 (/= '%') >>= \bs -> let s = BS.unpack bs in return [|StringElemTH s|]
                      , A.char '%' *> elemParser
                      ]
        (e :) <$> formatParser

elemParser :: A.Parser (Q Exp) -- LogFormatElemTH
elemParser = A.choice [ A.char '%' *> return [|StringElemTH "%"|]
                      , A.char 'm' *> return [|MessageElemTH|]
                      , A.char 's' *> return [|SeverityElemTH|]
                      , A.char 'd' *> datetimeParser >>= \f -> return [|DateTimeElemTH f|]
                      , A.char 'n' *> return [|ThreadElemTH|]
                      ]

datetimeParser :: A.Parser String
datetimeParser = BS.unpack <$> (A.char '(' *> datetimeParser' <* A.char ')')
  -- return $ if formatStr == ""
  --          then show
  --          else formatTime defaultTimeLocale (BS.unpack formatStr)

datetimeParser' :: A.Parser BS.ByteString
datetimeParser' = do
  s <- A.takeWhile (\c -> c /= '\\' && c /= ')')
  A.choice [ A.string "\\)" *> ((\r -> BS.concat [s, ")", r]) <$> datetimeParser')
           , liftA3 (\a b c -> a `BS.cons` b `BS.cons` c) (A.char '\\') A.anyChar datetimeParser'
           , return s
           ]


-- $(mat _) :: LogFormat
mat :: String -> Q Exp
mat bs = ListE <$> sequence (either (return . fail) id $ A.parseOnly formatParser $ BS.pack bs)
