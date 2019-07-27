{-# LANGUAGE MagicHash #-}
{-# LANGUAGE AllowAmbiguousTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fno-warn-simplifiable-class-constraints #-}
module Step4
  ( step4
  , runStep4
  ) where

import Capability.Reader as C
import Capability.Sink as C
import Capability.Source as C
import Capability.State as C
import Control.Lens
import Control.Monad (unless)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader
import Control.Type.Operator
import Data.Foldable (for_)
import Streaming.Prelude as S
import System.Directory
import System.FilePath
import qualified Data.Generics.Sum.Constructors as Generic

import Data.Set (Set)
import Data.IORef
import GHC.Generics (Generic)
import qualified Data.Set as Set
import GHC.Exts

data AllDirs
data Files 
data Dirs
data FilesCount 
data DirsCount 

step4 :: forall tag m .
  ( MonadIO m
  , HasSink Dirs FilePath m
  , HasSink Files FilePath m
  , HasState FilesCount Int m
  , HasState AllDirs (Set FilePath) m
  ) => FilePath -> m ()
step4 fp = do
  xs <- liftIO $ listDirectory fp
  for_ xs $ \x -> do
    isDir <- liftIO $ doesDirectoryExist $ fp </> x
    if isDir
    then do
      cp <- liftIO $ canonicalizePath $ fp </> x
      s <- get @AllDirs
      unless (cp `Set.member` s) $ do
        put @AllDirs (Set.insert cp s)
        C.yield @Dirs (fp </> x)
        step4 (fp </> x)
    else do
      C.modify @FilesCount (+1)
      C.yield @Files (fp </> x)

runStep4 :: IO ()
runStep4 = do
  c <- newIORef 0
  z <- S <$> newIORef (Set.empty) <*> pure c
  S.print $ runReaderT (runAppM (step4 ".")) z
  cnt <- readIORef c
  putStrLn $ "Files count: " ++ Prelude.show cnt

data S = S
  { cache :: IORef (Set FilePath)
  , count :: IORef Int
  } deriving Generic

data Elem = ElemFile FilePath | ElemDir FilePath
  deriving (Show)
  deriving (Generic)

newtype AppM a = AppM { runAppM :: ReaderT S (Stream (Of Elem) IO) a }
  deriving ( Functor, Applicative, Monad, MonadIO)
  deriving ( HasSource FilesCount Int
           , HasSink FilesCount Int
           , HasState FilesCount Int)
    via ReaderIORef
      $ Rename "count"
      $ Field "count" ()
      $ MonadReader
      $ ReaderT S 
      $ Stream (Of Elem) IO
  deriving ( HasSource AllDirs (Set FilePath)
           , HasSink AllDirs (Set FilePath)
           , HasState AllDirs (Set FilePath))
    via ReaderIORef
      $ Rename "cache" 
      $ Field "cache" ()
      $ MonadReader
      $ ReaderT S
      $ Stream (Of Elem) IO
  deriving (HasSink Dirs FilePath)
    via Lift
      $ ReaderT S
      $ Ctor "ElemDir" Dirs
      $ Stream (Of Elem) IO
  deriving (HasSink Files FilePath)
    via Lift
      $ ReaderT S
      $ Ctor "ElemFile" Files (Stream (Of Elem) IO)


instance
  (Generic.AsConstructor' ctor sum e, HasSink oldtag sum m, Generic sum)
  => HasSink oldtag e (Ctor ctor oldtag m) where
  yield_ _p = coerce @(e -> m ()) $ C.yield @oldtag
   . (review (Generic._Ctor' @ctor @sum))
