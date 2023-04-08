module IO.Atom where

import Prelude

import Control.Monad.Except (ExceptT(..))
import Effect (Effect)
import Effect.Aff (Aff, attempt, error)
import Fetch (fetch)
import Node.Encoding (Encoding(..))
import Node.FS.Aff (readTextFile, writeTextFile)
import Type.Alias (URL)
import Type.AffE (AffE)
import Type.AffE as AE
import Type.AppError (AppError(..))

getTextFromURL :: URL -> AffE String
getTextFromURL url = AE.tryAff NetworkError (fetch url {} >>= (\r -> r.text))-- ExceptT $ attempt (fetch url {} >>= (\r -> r.text))

readTextFromFile :: String -> AffE String
readTextFromFile = AE.tryAff FileError <<< readTextFile UTF8

writeTextToFile :: String -> String -> AffE Unit
writeTextToFile path text = AE.tryAff FileError $ writeTextFile UTF8 path text