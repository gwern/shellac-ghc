module Main where

import Commands(commands)
import GHCEval(Session, initializeGHC, eval)

import System.Console.Shell.Backend.Readline(readlineBackend)
import System.Console.Shell(ShellDescription(shellCommands, maxHistoryEntries,
                                             historyFile, historyEnabled, evaluateFunc, commandStyle),
                            CommandStyle(CharPrefixCommands), initialShellDescription, runShell)

{- 1.  Create a list of shell commands and an evaluation function
   2. Create a shell description (using mkShellDescription)
   3. Set up the initial shell state
   4. Run the shell (using runShell)
      runShell :: ShellDescription st -> ShellBackend bst -> st -> IO st -}

main :: IO ()
main = do s <- initializeGHC
          runShell (createShellDescription) (readlineBackend) s
          return ()

-- | Default shell configuration.
createShellDescription :: ShellDescription Session
createShellDescription =
  initialShellDescription {
    -- set the available commands, see also the function 'commands'
    shellCommands      = commands,

    -- commands are prefixed with a colon character
    commandStyle       = CharPrefixCommands '!',

    -- set the function called when something else than a command was entered
    evaluateFunc       = eval,

    -- command history is turned on
    historyEnabled     = True,

    -- the maximum number of commands remembered in one session
    maxHistoryEntries  = 1024,

    -- there is no file storing the commands of previous sessions
    historyFile        = Just "/home/gwern/.shellac" }
