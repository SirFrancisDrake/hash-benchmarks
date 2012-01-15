
{-# LANGUAGE RecordWildCards #-}

import Control.Applicative
import Data.Word
import Data.List
import qualified Data.ByteString as B
import System.Random

-- Вобще, наверное, это было бы удобнее делать через Arbitrary (из QuickCheck'a),
-- но я не умею им пользоваться.

data Params = Params
	{ key_size :: Int
  , val_size_pdf :: [(Int,Double)]
	} deriving Show

-- Это наборы данных.
-- Основная идея в том, чтобы понять насколько стоит заморачиваться с
-- компактным представлением ключей и значений.
params = do
  key_size <- [10,16,17] -- если сильно постараться VIN можно ужать до 10 байт
  -- Распределения размеров значений.
  -- Хотел сделать что-то вроде бета-распределения с параметрами (2,5), но не осилил:
  -- http://en.wikipedia.org/wiki/Beta_distribution
  -- Цифры приблизительные.
  val_size_pdf <- 
        [ [(128,0.7), (256,0.3)]
        , [(128,0.2), (256,0.8)]
        , [(128,0.3), (256,0.4), (384,0.3)]  
        , [(256,0.8), (384,0.2)]
        ]
  return $ Params key_size val_size_pdf


runPDF pdf = \x -> fst $ head $ dropWhile ((<x).snd) pdf'
  where
    pdf' = scanl1 (fmap.(+).snd) pdf

-- Генерация данных занимает изрядное время*,
-- возможно это надо как-то учитывать в бенчмарках.
-- [*] думаю, не в последнюю очередь из-за stdGen, где-то его ругали за тормознутость.
-- Пруфлинка нет.
sample :: Params -> [(B.ByteString,B.ByteString)]
-- Можно сделать более общий тип [([a],[b])], только осторожно, чтобы в `chunks` память не текла.
sample (Params {..}) = do
  keys   <- chunks (repeat key_size) . randoms <$> newStdGen
  valSzs <- map (runPDF val_size_pdf) . randoms <$> newStdGen
  vals   <- chunks valSzs . randoms <$> newStdGen
  return $ zip keys vals


chunks (len:lens) xs = str `seq` (str : chunks lens xs')
  where
    str = B.pack x
    (x,xs') = splitAt len xs


mapFst f (a,b) = (f a, b)

instance Random Word8 where
  randomR (a,b) = mapFst fromInteger . randomR (fromIntegral a, fromIntegral b)
  random = randomR (0,255)

