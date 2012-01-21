module TestData where

data TestStruct = TestStruct { ts_someNumber :: Int
                             , ts_someCode :: String
                             }
                    deriving (Show)

type TestPair = (String, TestStruct)

convert :: TestStruct -> TestPair
convert t = (ts_someCode t, t)
