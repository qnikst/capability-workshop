{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE PolyKinds #-}
module Step2
  ( step2
  , runStep2
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
data ListFiles

step2 :: forall m .
  ( MonadIO m
  , HasSink ListFiles String m
  , HasState AllDirs (Set FilePath) m) => FilePath -> m ()
step2 fp = do
  xs <- liftIO $ listDirectory fp
  for_ xs $ \x -> do
    isDir <- liftIO $ doesDirectoryExist $ fp </> x
    if isDir
    then do
      cp <- liftIO $ canonicalizePath $ fp </> x
      s <- get @AllDirs
      unless (cp `Set.member` s) $ do
        put @AllDirs (Set.insert cp s)
        step2 (fp </> x)
    else C.yield @ListFiles (fp </> x)

newtype AppM a = AppM {
  runAppM :: ReaderT (IORef (Set FilePath)) (Stream (Of String) IO) a
  } 
  deriving (Functor, Applicative, Monad, MonadIO)
  deriving ( HasSource AllDirs (Set FilePath)
           , HasSink AllDirs (Set FilePath)
           , HasState AllDirs (Set FilePath))
    via ReaderIORef
        $ MonadReader
        $ ReaderT (IORef (Set FilePath))
        $ Stream (Of String) IO
  deriving (HasSink ListFiles String)
    via Lift $ ReaderT (IORef (Set FilePath)) $ Stream (Of String) IO

runStep2 :: IO ()
runStep2 = do
  let appm = step2 "."
  ref <- newIORef Set.empty
  S.stdoutLn $ runReaderT (runAppM appm) ref
