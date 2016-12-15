{-# OPTIONS_GHC -Wall #-}
{-# OPTIONS_GHC -Wno-missing-signatures #-}
{-# OPTIONS_GHC -fwarn-incomplete-uni-patterns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BangPatterns #-}
module Day15Extended where

import qualified Data.ByteString.Char8 as BS

import Utils

md5sums c = iterate md5 (BS.replicate 32 c)

count0 :: BS.ByteString -> [Int]
count0 bs = filter (\i -> i < (32 - 8)) (BS.elemIndices '0' bs)

findIt :: Int -> [BS.ByteString] -> (Int, BS.ByteString)
findIt !time (x:xs) = let zeros = count0 x
                          others = take 7 xs
                      in if any (checkZeros others) zeros
                         then (time, x)
                         else findIt (time + 1) xs

checkZeros :: [BS.ByteString] -> Int -> Bool
checkZeros items pos = all (\item -> BS.index item pos == '0') items


res = let (offset, sum) = findIt 0 (md5sums '0')
      in do
  print (offset, sum)
  mapM_ (BS.putStrLn) $ take 8 (iterate md5 sum)
res' = (40611147,"ff538c1bc116faa0288ee54d57c31922") :: (Int, BS.ByteString)


{-

result :


(204947639,"f1773abd2e79f01510f9c2a2b4336136")

(which means t = 204947639 - 1

             *
f1773abd2e79f01510f9c2a2b4336136
2a13793c429cd0fe9f803d12341f9dfd
4a48eedba347e0fba6a4b6d5d977736c
01e4ca27df8a3059ce2377cb81ed7ea8
e43d5eca6331d0b88dabb8c20aa47106
47462d6606bd20a74d97a1e7aea23070
12116b454ccd8049a4dc2cd882bd9f3f
0132b44673c240e3d51e0cde2bcd2921

-}
