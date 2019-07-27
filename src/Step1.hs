{-# LANGUAGE AllowAmbiguousTypes #-}     -- (1)
{-# LANGUAGE FlexibleContexts #-}        -- (2)
{-# LANGUAGE ScopedTypeVariables #-}     -- (3)
{-# LANGUAGE TypeApplications #-}        -- (4)
module Step1
  ( step1
  , runStep1
  ) where

import Capability.Sink as C
import Control.Monad.IO.Class
import Data.Foldable (for_)
import Streaming.Prelude as S
import System.Directory
import System.FilePath

-- 
--       sink
--     -------- 
--
--  m => yield {value}
--
--

step1 :: forall tag m
    . (MonadIO m, HasSink tag String m)
    => FilePath -> m ()
step1 fp = do
  xs <- liftIO $ listDirectory fp
  for_ xs $ \x -> do
    isDir <- liftIO $ doesDirectoryExist $ fp </> x
    if isDir
    then step1 @tag (fp </> x)
    else C.yield @tag (fp </> x)

runStep1 :: IO ()
runStep1 = do
  let s = step1 "."  -- Stream (Of String) IO ()
  S.stdoutLn s
