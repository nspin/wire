module QC.Common where

import Test.QuickCheck
import Control.Monad
import Data.Bifunctor
import qualified Data.ByteString as B
import qualified Data.ByteString.Lazy as L

inputs = arbitraryInput 100 10
chunks = arbitraryChunk 100
nonEmptyInputs = arbitraryNonEmptyInput 100 10

type Chunk = B.ByteString -- /= mempty
type Input = [Chunk]

type InputPart = (Chunk, Input)

solder :: InputPart -> Chunk -> InputPart -> Input
solder (lc, li) m (rc, ri) = li <> [lc <> m <> rc] <> ri

inputLength :: Input -> Int
inputLength = sum . map B.length

-- append to right at chunk boundaries, exept for at beginning
-- required: i <= inputLength input
inputSplitAt :: Int -> Input -> (InputPart, InputPart)
inputSplitAt i (chunk:chunks) = case i of
    0          -> ((mempty, mempty), (chunk, chunks))
    _ | i' > 0 -> first (second (<> [chunk])) (inputSplitAt i' chunks)
    _          -> ((l, []), (r, chunks))
  where
    i' = i - B.length chunk
    (l, r) = B.splitAt i chunk

shrinkInput :: Input -> [Input]
shrinkInput = shrinkList shrinkChunk

shrinkNonEmptyInput :: Input -> [Input]
shrinkNonEmptyInput = shrinkNonEmptyList shrinkChunk

shrinkNonEmptyList :: (a -> [a]) -> [a] -> [[a]]
shrinkNonEmptyList shr xs =
    concat [ removes k n xs | k <- takeWhile (> 0) (iterate (`div` 2) n) ]
    <> shrinkOne xs
  where
    n = length xs
    shrinkOne [] = []
    shrinkOne (x:xs) = [ x':xs | x' <- shr x ] <> [ x:xs' | xs' <- shrinkOne xs ]
    removes k n xs
        | k > n     = []
        | null xs2  = []
        | otherwise = xs2 : map (xs1 ++) (removes k (n - k) xs2)
      where
        xs1 = take k xs
        xs2 = drop k xs

shrinkChunk :: Chunk -> [Chunk]
shrinkChunk chunk
    | B.length chunk == 1 = []
    | otherwise = [ a <> b | (a, b) <- zip (B.inits chunk) (tail (B.tails chunk)) ]

arbitraryInput :: Int -> Int -> Gen Input
arbitraryInput maxChunkSize maxChunks = choose (0, maxChunks) >>= arbitraryInputOf maxChunkSize

arbitraryNonEmptyInput :: Int -> Int -> Gen Input
arbitraryNonEmptyInput maxChunkSize maxChunks = choose (1, maxChunks) >>= arbitraryInputOf maxChunkSize

arbitraryInputOf :: Int -> Int -> Gen Input
arbitraryInputOf maxChunkSize = flip replicateM (arbitraryChunk maxChunkSize)

arbitraryChunk :: Int -> Gen Chunk
arbitraryChunk maxChunkSize = choose (1, maxChunkSize) >>= arbitraryChunkOf

arbitraryChunkOf :: Int -> Gen Chunk
arbitraryChunkOf n = B.pack <$> vectorOf n (choose (32, 126)) -- printable ascii pts for easier debugging

arbitrarySplit :: Input -> Gen (InputPart, InputPart)
arbitrarySplit input = flip inputSplitAt input <$> choose (0, inputLength input)
