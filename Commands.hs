module Commands where

import Control.Monad.State (Monad(return, (>>), (>>=)), MonadIO(..))
import Data.List ((++))
import Parse as P (getCmd, getArg)
import System.Cmd (rawSystem)
import System.Console.Shell (ShellCommand, cmd, exitCommand)
import System.Console.Shell.ShellMonad (Sh, shellPutStrLn)
import System.Exit (ExitCode)
import System.IO (IO, hGetContents)
import System.Process (runInteractiveProcess, waitForProcess)

-------------------------
-- Commands
-------------------------
commands :: [ShellCommand (a)]
commands =  [cmd "top" (top) "run top",
             cmd "echo" (echo) "run echo",
             cmd "fork" (doFork "echo foo && cat none; top -n 1") "external command",
             cmd "" (doFork) "Run a specified external command.",
             exitCommand "quit"]

doExec :: String -> Sh (a) ()
doExec a = liftIO $ exec a >> return ()

top :: Sh (a) ()
top = liftIO $ exec "top" >> return ()

echo :: String -> Sh (a) ()
echo s = liftIO $ exec ("echo foo bar " ++  s) >> return ()

-- | Wrapper around 'fork' to make it usable by 'cmd'.
doFork :: String -> Sh (a) ()
doFork cmd = liftIO (fork cmd) >>= shellPutStrLn

-- | For when you want to evaluate through 'sh'.
fork :: String -> IO String
fork string = do
   (_,out,err,pid) <- runInteractiveProcess "/bin/sh" ["-c",string] Nothing Nothing
   results <- (hGetContents out)
   errors <- (hGetContents err)
   -- ... until the output is exhausted and the program has finished
   waitForProcess pid
   return $ results ++ errors

-- | Basic command execution.
exec :: String -> IO ExitCode
exec e = rawSystem (P.getCmd e) (P.getArg e)
