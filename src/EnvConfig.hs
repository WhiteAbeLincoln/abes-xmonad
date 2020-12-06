{-# LANGUAGE TemplateHaskell #-}
module EnvConfig (
  getConfigText
) where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addDependentFile, Lift(..))
import CompileEnvironment (getFileContents, lookupCompileEnv)
import Data.Maybe (fromMaybe)
import Control.Applicative (Alternative((<|>)))

getConfigText fileKey srcKey = do
  filePath <- lookupCompileEnv fileKey
  src <- lookupCompileEnv srcKey
  fileSrc <- mapM getFileContents (filePath <|> Just "/home/abe/Documents/Projects/dotfiles/packages/xmonad/config.json")
  stringE $ fromMaybe "{}" $ src <|> fileSrc
