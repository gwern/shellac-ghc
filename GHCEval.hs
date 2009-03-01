{-# OPTIONS_GHC -fth #-}
module GHCEval where

import Control.Monad.State (Monad(return, (>>)), mapM, MonadIO(..))
import Data.Dynamic (fromDyn)
import qualified DynFlags
import qualified GHC
import System.Console.Shell.ShellMonad (Sh, shellPutStrLn, shellPutErrLn, getShellSt)
import System.IO.Unsafe (unsafePerformIO)
import Data.List
import System.Directory
import Control.Monad -- (join, liftM)
import System.FilePath

import Commands (doExec, exec)
import TH

--------------------------------
-- Initialization of GHC session
--------------------------------
type Session = GHC.Session -- for export

-- | default configuration
ghcPath :: String
ghcPath = unsafePerformIO $(path) -- unsafePerformIO $ ghcDirectory

initializeGHC :: IO GHC.Session
initializeGHC = GHC.defaultErrorHandler DynFlags.defaultDynFlags $ do
                             -- Where's our GHC-as-library? There you are!
                             session <- GHC.newSession (Just ghcPath)
                             -- Setup boilerplate
                             df      <- GHC.getSessionDynFlags session
                             -- Running interpreted means less compiled stuff all over the place
                             let df' = df{GHC.hscTarget=GHC.HscInterpreted}
                             GHC.setSessionDynFlags session df'
                             GHC.load session GHC.LoadAllTargets
                             let modules = ["Prelude", "Data.Maybe", "Control.Monad"]
                             loadModules session modules
                             return session

-- Perhaps don't blindly load modules and claim they all succeeded...
loadModules :: GHC.Session -> [String] -> IO ()
loadModules session mod = do modules <- mapM (\m -> GHC.findModule session (GHC.mkModuleName m) Nothing) mod
                             GHC.setContext session [] modules

---------------------------------------
-- Parsing and evaluation
---------------------------------------
eval :: String -> Sh (GHC.Session) ()
eval "" = return ()
eval ('!':as) = doExec as
eval as = do s <- getShellSt
             dyn <- liftIO $ GHC.dynCompileExpr s $ "show (" ++ as ++ ")"
             case dyn of
               Nothing -> shellPutErrLn "Failed to compile expression!"
               Just d  -> shellPutStrLn $ fromDyn d $ error "Dynamic expression failed to produce a string."

{-# NOINLINE evalSplit #-}
evalSplit :: String -> IO GHC.Session -> IO GHC.Session
evalSplit ('!':bs) s = exec (tail bs) >> s
evalSplit bs s = GHC.runStmt (unsafePerformIO s) bs GHC.SingleStep >> s

spl :: String -> [String]
spl = split ">>"

{- | Utility function.
   > split ">>" "foo >> bar >> baz" ~> ["foo","bar","baz"] -}
split :: String -> String -> [String]
split m ns = map unwords $ wordsBy m $ words ns
             where wordsBy n = map (takeWhile (/= n)) . takeWhile (not . null) . iterate (drop 1 . dropWhile (/= n))
