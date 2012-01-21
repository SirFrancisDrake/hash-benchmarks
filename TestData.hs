module TestData where

data TestStruct = TestStruct { ts_someNumber :: Int
                             , ts_someCode :: String
                             }
                    deriving (Show)

type TestPair = (String, TestStruct)

convert :: TestStruct -> TestPair
convert t = (ts_someCode t, t)

genData :: Int -> IO [TestPair]
genData size = do
    let genStruct i = TestStruct i (show i)
    return $ map (\i -> (show i, genStruct i) ) [1..size]

