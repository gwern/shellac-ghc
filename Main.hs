import qualified GHC -- (LoadAllTargets, Session, defaultErrorHandler,
                     -- findModule, getSessionDynFlags, load, mkModuleName,
                     -- newSession, runStmt, setContext, setSessionDynFlags)
import Control.Monad
import Data.List
-- import GHCEval (ghcPath)
import Parse as P
import System.Cmd (rawSystem)
import System.Console.Readline
import System.Console.Shell
import System.Console.Shell.Backend.Readline
import System.Console.Shell.ShellMonad
import System.Exit
import System.IO
import System.Process
import qualified DynFlags
import Control.Concurrent
import System.IO.Unsafe
{-
   1.  Create a list of shell commands and an evaluation function
   2. Create a shell description (using mkShellDescription)
   3. Set up the initial shell state
   4. Run the shell (using runShell)
-}
-- runShell :: ShellDescription st -> ShellBackend bst -> st -> IO st

ghcPath = "/home/gwern/bin/lib/ghc-6.9.20071201"

main :: IO ()
main = do -- s <- initializeGHC
          runShell (description) (readlineBackend) (initializeGHC)
          return ()
                 where
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
                                     loadModule session modules
                                     return session
                                         where
                                           loadModule :: GHC.Session -> [String] -> IO ()
                                           loadModule session mod = do modules <- mapM (\m -> GHC.findModule session (GHC.mkModuleName m) Nothing) mod
                                                                       GHC.setContext session [] modules >> return ()

-- mkShellDescription :: [ShellCommand st] -> (String -> Sh st ()) -> ShellDescription st
description = mkShellDescription [] (eval)

-- eval :: String -> Sh st ()
eval a = modifyShellSt (parser a)
-- liftM (parser a) s >> putShellSt s
--liftM (parser a) s >> return (s ())

parser :: String -> IO GHC.Session -> IO GHC.Session
-- parser "" s = return s -- Ignore it and continue on
parser x s = mapM (evalSplit s) (spl x) >> s
-- if (x `isPrefixOf` "quit") then return s -- Let'em quit!

evalSplit :: IO GHC.Session -> String -> IO ()
evalSplit s x = if (head x /= '!') then GHC.runStmt (unsafePerformIO s) x GHC.SingleStep >> return ()
                                    else exec (tail x) >> return ()

spl :: String -> [String]
spl = split ">>"

{- | Utility function.
   > split ">>" "foo >> bar >> baz" ~> ["foo","bar","baz"] -}
split :: String -> String -> [String]
split m ns = map unwords $ wordsBy m $ words ns
             where wordsBy n = map (takeWhile (/= n)) . takeWhile (not . null) . iterate (drop 1 . dropWhile (/= n))

-- | For sh evaluation; use forkIO eventually.
fork :: String -> IO String
fork string = do
   (inp,out,err,pid) <- runInteractiveProcess "/bin/sh" ["-c ", string] Nothing Nothing
   result <- (hGetLine out)
   return result

-- | Basic command execution.
exec :: String -> IO ExitCode
exec e = rawSystem (P.getCmd e) (P.getArg e)

