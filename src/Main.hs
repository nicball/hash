{-# LANGUAGE DeriveAnyClass #-}

import Data.List (singleton)
import Data.ByteString.Lazy (ByteString)
import qualified Data.ByteString.Lazy.UTF8 as ByteString hiding (length)
import qualified Data.ByteString.Lazy as ByteString
import qualified System.Process as Process
import Control.Exception (Exception, throwIO)
import Control.Concurrent (forkIO)
import Control.Monad (when)
import System.IO (hClose)

data Command a b where
  PureCommand :: (a -> IO b) -> Command a b
  ShellCommand :: ShellCommand a b -> Command a b
  Pipe :: Command a c -> Command c b -> Command a b

data ShellCommand a b = MkShellCommand
  { shellCommandLine :: String
  , shellCommandInputAdaptor :: a -> ByteString
  , shellCommandOutputAdaptor :: ByteString -> b
  }

class Adaptable a b where
  adapt :: a -> b

instance Adaptable a a where
  adapt = id

-- instance Show a => Adaptable a String where
--   adapt = show

instance Adaptable a [a] where
  adapt = singleton

instance Adaptable a (Maybe a) where
  adapt = Just

instance Adaptable ByteString [ByteString] where
  adapt = ByteString.lines

instance Adaptable ByteString () where
  adapt = const ()

instance Adaptable () ByteString where
  adapt = const ByteString.empty

(.|) :: Adaptable b c => Command a b -> Command c d -> Command a d
(.|) x y = x `Pipe` PureCommand (pure . adapt) `Pipe` y

writeTo :: FilePath -> Command ByteString ()
writeTo = PureCommand . ByteString.writeFile

readFrom :: FilePath -> Command () ByteString
readFrom = PureCommand . const . ByteString.readFile

(.>) :: Adaptable b ByteString => Command a b -> FilePath -> Command a ()
x .> path = x .| writeTo path

(.<) :: Adaptable ByteString a => Command a b -> FilePath -> Command () b
x .< path = readFrom path .| x

shell :: String -> Command ByteString ByteString
shell cmd = ShellCommand $ MkShellCommand cmd id id

retcode :: String -> Command ByteString Int
retcode cmd = ShellCommand $ MkShellCommand ("{ " ++ cmd ++ "; } > /dev/null; echo $?") id (read . ByteString.toString)

data CommandError = CommandError ByteString
  deriving (Show, Exception)

runCommand :: Command a b -> a -> IO b
runCommand (PureCommand f) a = f a
runCommand (ShellCommand (MkShellCommand cmd input output)) a = do
  (Just stdin, Just stdout, Just stderr, _) <- Process.createProcess (Process.shell cmd)
    { Process.std_in = Process.CreatePipe, Process.std_out = Process.CreatePipe, Process.std_err = Process.CreatePipe }
  forkIO $ ByteString.hPut stdin (input a) >> hClose stdin
  err <- ByteString.hGetContents stderr
  out <- output <$> ByteString.hGetContents stdout
  forkIO . when (ByteString.length err /= 0) . throwIO . CommandError $ err
  pure out
runCommand (Pipe x y) a = runCommand x a >>= runCommand y

(.$) :: Adaptable c a => Command a b -> c -> IO b
f .$ a = runCommand f (adapt a)
