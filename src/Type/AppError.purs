module Type.AppError where

import Prelude
import Effect.Exception (Error)
import Data.Show

data AppError = FileError Error | JSONParseError | MissingAPIKey | NetworkError Error

instance showAppError :: Show AppError where
  show = case _ of
    FileError e -> "FileError " <> show e
    JSONParseError -> "JSONParseError"
    MissingAPIKey -> "MissingAPIKey"
    NetworkError e -> "NetworkError " <> show e