module TestData where

data TestStruct = TestStruct { ts_someNumber :: Int
                             , ts_someCode :: String
                             }
                    deriving (Show)

-- size of TestStruct should be 5 words + 1 word for each word in string iirc
-- meaning, 7 words for a total of 56 bytes, round it to 64, amiright?
-- hashmap thinks 

type TestPair = (String, TestStruct)

convert :: TestStruct -> TestPair
convert t = (ts_someCode t, t)

genData :: Int -> IO [TestPair]
genData size = do
    let genStruct i = TestStruct i ("Num = " ++ show i)
    return $ map (\i -> (show i, genStruct i) ) [1..size]

