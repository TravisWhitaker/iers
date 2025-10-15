{-# LANGUAGE RecordWildCards
           , TupleSections
           #-}

module Main where

import Codec.Serialise

import qualified Data.Attoparsec.ByteString as A

import Data.Bifunctor

import qualified Data.ByteString as BS

import Data.Either

import qualified Data.IntMap.Strict as IM

import Data.List

import System.Environment

import System.Directory

import System.FilePath

import Data.IERS

testFile :: FilePath -> IO (Either (FilePath, String) BulletinA)
testFile fp = first (fp,) . chk . A.parseOnly parseBulletinA <$> BS.readFile fp
    where chk (Right BulletinA{..})
            | IM.null baDUT1 = Left "Empty DUT1 table"
            | IM.null baREOP = Left "Empty REOP table"
            | IM.null baPEOP = Left "Empty PEOP table"
          chk r = r

runArgs :: [String] -> IO ()
runArgs [testDirPath] = do
    fns <- listDirectory testDirPath
    let fullFns = (testDirPath </>) <$> filter (".txt" `isSuffixOf`) fns
    (errs, goods) <- partitionEithers <$> traverse testFile fullFns
    case errs of
        [] -> putStrLn "Parse all good."
        _ -> fail ("failures: " <> show errs)
    let bat = IM.fromList (fmap (\b -> (dayToKey (baPublishedDate b), b)) goods)
        batS = serialise bat
        batSS = deserialise batS
    if bat == batSS
    then putStrLn "Roundtrip all good."
    else fail "Roundtrip fail."

runArgs _ = putStrLn "usage: ./parse (test_dir_path)"

main :: IO ()
main = getArgs >>= runArgs
