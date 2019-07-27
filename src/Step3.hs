{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
module Step3
  ( step3
  , runStep3
  ) where

import Capability.Reader as C
import Capability.Sink as C
import Capability.Source as C
import Capability.State as C
import Control.Monad (unless)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Type.Operator
import Data.Foldable (for_)
import Streaming.Prelude as S
import System.Directory
import System.FilePath

import Data.Set (Set)
import Data.IORef
import GHC.Generics (Generic)
import qualified Data.Set as Set

data AllDirs
data Files 
data FilesCount 

step3 :: forall tag m .
  ( MonadIO m
  , HasSink Files String m
  , HasState FilesCount Int m
  , HasState AllDirs (Set FilePath) m
  ) => FilePath -> m ()
step3 fp = do
  xs <- liftIO $ listDirectory fp
  for_ xs $ \x -> do
    isDir <- liftIO $ doesDirectoryExist $ fp </> x
    if isDir
    then do
      cp <- liftIO $ canonicalizePath $ fp </> x
      s <- get @AllDirs
      unless (cp `Set.member` s) $ do
        put @AllDirs (Set.insert cp s)
        step3 (fp </> x)
    else do
      C.modify @FilesCount (+1)
      C.yield @Files (fp </> x)

runStep3 :: IO ()
runStep3 = do
  c <- newIORef 0
  z <- S <$> newIORef (Set.empty) <*> pure c
  S.stdoutLn $ runReaderT (runAppM (step3 ".")) z
  cnt <- readIORef c
  putStrLn $ "Files count: " ++ Prelude.show cnt

data S = S
  { cache :: IORef (Set FilePath)
  , count :: IORef Int
  } deriving Generic

newtype AppM a = AppM { runAppM :: ReaderT S (Stream (Of String) IO) a }
  deriving (Functor, Applicative, Monad, MonadIO)
  deriving ( HasSource FilesCount Int
           , HasSink FilesCount Int
           , HasState FilesCount Int)
    via ReaderIORef
      $ Rename "count" 
      $ Field "count" () 
      $ MonadReader
      $ ReaderT S (Stream (Of String) IO)
  deriving ( HasSource AllDirs (Set FilePath)
           , HasSink AllDirs (Set FilePath)
           , HasState AllDirs (Set FilePath))
    via ReaderIORef
      $ Rename "cache" 
      $ Field "cache" ()
      $ MonadReader
      $ ReaderT S (Stream (Of String) IO)
  deriving (HasSink Files String)
    via Lift
      $ ReaderT S (Stream (Of String) IO)
