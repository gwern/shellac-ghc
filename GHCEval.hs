module GHCEval where
import qualified GHC -- (LoadAllTargets, Session, defaultErrorHandler,
                     -- findModule, getSessionDynFlags, load, mkModuleName,
                     -- newSession, runStmt, setContext, setSessionDynFlags)
import qualified DynFlags
import System.IO.Unsafe
import System.Console.Shell.ShellMonad
import Data.Dynamic
import Control.Monad.State

import Commands (exec)

--------------------------------
-- Initialization of GHC session
--------------------------------
type Session = GHC.Session -- for export

-- | default configuration
ghcPath :: String
ghcPath = "/home/gwern/bin/lib/ghc-6.9.20071201"

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
                             return ()

---------------------------------------
-- Parsing and evaluation
---------------------------------------
eval :: String -> Sh (GHC.Session) ()
eval "" = return ()
eval a = do s <- getShellSt
            dyn <- liftIO $ GHC.dynCompileExpr s $ "show (" ++ a ++ ")"
            case dyn of
              Nothing -> shellPutErrLn "Failed to compile expression!"
              Just d  -> shellPutStrLn $ fromDyn d $ error "Dynamic expression failed to produce a string."

parser :: String -> IO GHC.Session -> IO GHC.Session
parser x s = putStrLn x >> s

{-# NOINLINE evalSplit #-}
evalSplit :: String -> IO GHC.Session -> IO GHC.Session
evalSplit x s = if (head x /= '!') then GHC.runStmt (unsafePerformIO s) x GHC.SingleStep >> s
                                    else exec (tail x) >> s

spl :: String -> [String]
spl = split ">>"

{- | Utility function.
   > split ">>" "foo >> bar >> baz" ~> ["foo","bar","baz"] -}
split :: String -> String -> [String]
split m ns = map unwords $ wordsBy m $ words ns
             where wordsBy n = map (takeWhile (/= n)) . takeWhile (not . null) . iterate (drop 1 . dropWhile (/= n))
