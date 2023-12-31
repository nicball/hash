{-# LANGUAGE DeriveAnyClass, GADTs #-}

import Control.Concurrent (forkIO)
import Control.Exception (Exception, throwIO)
import Control.Monad (when)
import Data.ByteString.Lazy (ByteString)
import Data.List (singleton)
import Data.Ratio (numerator, denominator)
import qualified Data.ByteString.Lazy as ByteString
import qualified Data.ByteString.Lazy.Char8 as ByteString (unlines, putStrLn)
import qualified Data.ByteString.Lazy.UTF8 as ByteString hiding (length)
import qualified System.Posix.Directory as Unix
import qualified System.Posix.Files as Unix
import qualified System.Posix.Types as Unix
import qualified System.Posix.User as Unix
import qualified System.Process as Process
import System.IO (hClose)

infix 2 .$
infixl 3 .>
infixl 3 .<
infixr 4 .|
infixr 4 `Pipe`

data Command a b where
  PureCommand :: (a -> IO b) -> Command a b
  ShellCommand :: String -> Command ByteString ByteString
  Pipe :: Command a c -> Command c b -> Command a b

data CommandError = CommandError ByteString
  deriving (Show, Exception)

runCommand :: Command a b -> a -> IO b
runCommand (PureCommand f) a = f a
runCommand (ShellCommand cmd) a = do
  (Just stdin, Just stdout, Just stderr, _) <- Process.createProcess (Process.shell cmd)
    { Process.std_in = Process.CreatePipe, Process.std_out = Process.CreatePipe, Process.std_err = Process.CreatePipe }
  forkIO $ ByteString.hPut stdin a >> hClose stdin
  err <- ByteString.hGetContents stderr
  out <- ByteString.hGetContents stdout
  forkIO . when (ByteString.length err /= 0) . throwIO . CommandError $ err
  pure out
runCommand (Pipe x y) a = runCommand x a >>= runCommand y

instance Functor (Command a) where
  fmap f (PureCommand g) = PureCommand (fmap f <$> g)
  fmap f c@(ShellCommand _) = c `Pipe` PureCommand (pure . f)
  fmap f (x `Pipe` y) = x `Pipe` fmap f y

instance Applicative (Command a) where
  pure a = PureCommand (pure . const a)
  f <*> a = PureCommand $ (<*>) <$> runCommand f <*> runCommand a

instance Monad (Command a) where
  m >>= f = PureCommand $ \i -> runCommand m i >>= flip runCommand i . f

class Adaptable a b where
  adapt :: a -> b

instance Adaptable a a where
  adapt = id

instance Adaptable a [a] where
  adapt = singleton

instance Adaptable a (Maybe a) where
  adapt = Just

instance Adaptable ByteString () where
  adapt = const ()

instance Adaptable () ByteString where
  adapt = const ByteString.empty

instance Adaptable a ByteString => Adaptable a [ByteString] where
  adapt = ByteString.lines . adapt

instance Adaptable a ByteString => Adaptable [a] ByteString where
  adapt = ByteString.unlines . map adapt

class ToScreen a where
  toScreen :: a -> ByteString

instance ToScreen ByteString where
  toScreen = id

instance ToScreen a => ToScreen [a] where
  toScreen = ByteString.unlines . map toScreen

instance ToScreen () where
  toScreen = const ByteString.empty

instance ToScreen Int where
  toScreen = ByteString.fromString . show

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
shell = ShellCommand

hs :: (a -> IO b) -> Command a b
hs = PureCommand

retcode :: String -> Command ByteString Int
retcode cmd = ShellCommand ("{ " ++ cmd ++ "; } > /dev/null; echo $?") .| hs (pure . read . ByteString.toString)

(.$) :: (Adaptable c a, ToScreen b) => Command a b -> c -> IO ()
f .$ a = ByteString.putStrLn =<< toScreen <$> runCommand f (adapt a)

data FileSpec = FileSpec
  { fileName :: String
  , fileOwner :: User
  , fileGroup :: Group
  , fileSize :: Int
  , fileMTime :: Int
  }

data User = User
  { userID :: Int
  , userName :: String
  }

data Group = Group
  { groupID :: Int
  , groupName :: String
  }

getUser :: Unix.UserID -> IO User
getUser id = (User (fromIntegral id) . Unix.userName <$>) . Unix.getUserEntryForID $ id

getGroup :: Unix.GroupID -> IO Group
getGroup id = (Group (fromIntegral id) . Unix.groupName <$>) . Unix.getGroupEntryForID $ id

getFileSpec :: FilePath -> IO FileSpec
getFileSpec path = do
  stat <- Unix.getFileStatus path
  owner <- getUser . Unix.fileOwner $ stat
  group <- getGroup . Unix.fileGroup $ stat
  let size = fromIntegral . Unix.fileSize $ stat
      mtime = fromIntegral . trunc . toRational . Unix.modificationTime $ stat
  pure $ FileSpec path owner group size mtime
  where trunc a = numerator a `div` denominator a

instance ToScreen FileSpec where
  toScreen fs = ByteString.fromString $
    userName (fileOwner fs) ++ " " ++ groupName (fileGroup fs) ++ " " ++ show (fileSize fs) ++ " " ++ show (fileMTime fs) ++ " " ++ fileName fs

instance Adaptable FileSpec ByteString where
  adapt = toScreen

ls :: FilePath -> Command () [FileSpec]
ls path = hs $ \() -> Unix.openDirStream path >>= getDirContent >>= traverse getFileSpec
  where
    getDirContent s = do
      p <- Unix.readDirStream s
      if null p
        then pure []
        else getDirContent s >>= pure . (p :)
