module Step0
  ( step0
  , runStep0
  ) where

import System.Directory
  ( listDirectory       -- Get all files in dir except for . and ..
  , doesDirectoryExist  -- If directory exist or not
  )
import System.FilePath ( (</>) )

-- step0 :: FilePath -> IO [FilePath] 
-- step0 fp = listDirectory fp

step0 :: FilePath -> IO [FilePath]
step0 fp = do
  xs <- listDirectory fp
  let loop c [] = pure c
      loop c (x:xs) = do
        isDir <- doesDirectoryExist $ fp </> x
        if isDir
        then do next <- step0 (fp </> x)
                loop (next++c) xs
        else loop ((fp </> x):c) xs
  loop [] xs

runStep0 = step0 "."
