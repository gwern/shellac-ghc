module Commands where
import Data.List
import Parse as P
import System.Cmd (rawSystem)
import Control.Monad.State
import System.Console.Shell
import System.Console.Shell.ShellMonad
import System.Exit
import System.IO
import System.Process

-------------------------
-- Commands
-------------------------
commands :: [ShellCommand (a)]
commands =  [cmd "top" (top) "run top",
             cmd "echo" (echo) "run echo",
             cmd "fork" (doFork "echo foo && cat none") "external command",
            exitCommand "quit"]

top :: Sh (a) ()
top = liftIO $ exec "top" >> return ()

echo :: Sh (a) ()
echo = liftIO $ fork ("echo 'foo'") >> return ()

-- | Wrapper around 'fork' to make it usable by 'cmd'.
doFork :: String -> Sh (a) ()
doFork cmd = liftIO (fork cmd) >>= shellPutStrLn

-- | For when you want to evaluate through 'sh'.
fork :: String -> IO String
fork string = do
   (_,out,err,pid) <- runInteractiveProcess "/bin/sh" ["-c",string] Nothing Nothing
   results <- (hGetContents out)
   errors <- (hGetContents err)
   -- force output to be read strictly...
   length results `seq` do
       -- ... until the output is exhausted and the program has finished
       waitForProcess pid
   length errors `seq` do
       waitForProcess pid
   return $ results ++ errors

-- | Basic command execution.
exec :: String -> IO ExitCode
exec e = rawSystem (P.getCmd e) (P.getArg e)

-- | Pipeline operator; left-to-right instead of ($)'s right-to-left
(|>) :: a -> (a -> b) -> b
(|>) = flip ($)
