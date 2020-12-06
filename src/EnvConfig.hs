{-# LANGUAGE TemplateHaskell #-}
module EnvConfig (
  getConfigText
) where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addDependentFile, Lift(..))
import CompileEnvironment (getFileContents, lookupCompileEnv)
import Data.Maybe (fromMaybe)
import Control.Applicative (Alternative((<|>)))
import Control.Monad (join)

getConfigText fileKey srcKey = do
  filePath <- lookupCompileEnv fileKey
  src <- lookupCompileEnv srcKey
  fileSrc <- join <$> mapM getFileContents filePath
  stringE $ fromMaybe "{}" $ src <|> fileSrc
