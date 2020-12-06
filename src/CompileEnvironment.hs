{-# LANGUAGE TemplateHaskell #-}
module CompileEnvironment
(
  lookupCompileEnv
, lookupCompileEnv'
, lookupCompileEnvExp
, getCompileEnv
, getCompileEnvExp
, getFileContents
) where
import Language.Haskell.TH
import Language.Haskell.TH.Syntax (addDependentFile, Lift(..))
import System.Environment (getEnvironment)
import Control.Monad
import qualified Data.Text as T
import qualified Data.Text.IO as T
import Data.Maybe (fromMaybe)
import Control.Exception


-- Compile Time Functions {{{
-- | Looks up a compile-time environment variable.
lookupCompileEnv :: String -> Q (Maybe String)
lookupCompileEnv key = lookup key `liftM` runIO getEnvironment

lookupCompileEnv' key def = fromMaybe def `liftM` lookupCompileEnv key

-- | Looks up a compile-time environment variable. The result is a TH
-- expression of type @Maybe String@.
lookupCompileEnvExp :: String -> Q Exp
lookupCompileEnvExp = (`sigE` [t| Maybe String |]) . lift <=< lookupCompileEnv
    -- We need to explicly type the result so that things like `print Nothing`
    -- work.

-- | Looks up an compile-time environment variable and fail, if it's not
-- present.
getCompileEnv :: String -> Q String
getCompileEnv key =
  lookupCompileEnv key >>=
  maybe (fail $ "Environment variable " ++ key ++ " not defined") return

-- | Looks up an compile-time environment variable and fail, if it's not
-- present. The result is a TH expression of type @String@.
getCompileEnvExp :: String -> Q Exp
getCompileEnvExp = lift <=< getCompileEnv

readTextFile file = do
  contents <- T.readFile file
  return $ Just (T.unpack . T.strip $ contents)

guardIOAction = flip catch $ \e -> const (return Nothing) (e :: IOException)

-- | Loads the content of a file as a string constant expression.
-- The given path is relative to the source directory.
getFileContents file = do
  addDependentFile file
  runIO $ guardIOAction $ readTextFile file
-- }}}
