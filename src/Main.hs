module Main (main) where

import Data.List (intercalate)
import qualified Data.Map.Strict as M
import IterLara (Row (..), Table (..), ext, for, iter, fromList, relaxedJoin, strictJoin, tableUnion)

renderRow :: (Show a) => [Char] -> Row a -> String
renderRow schema (Row xs) =
  "{"
    <> intercalate ", " (zipWith (\n x -> [n] <> "=" <> show x) schema xs)
    <> "}"

prettyTable :: (Show k, Show v) => Table k v -> String
prettyTable (Table ks vs rs) =
  "key=" <> intercalate "," (fmap (: []) ks) <> "  value=" <> intercalate "," (fmap (: []) vs)
    <> "\n"
    <> unlines (fmap (\(k, v) -> renderRow ks k <> " -> " <> renderRow vs v) (M.toAscList rs))

vecTable :: [v] -> Table Int v
vecTable xs =
  fromList
    ['i']
    ['v']
    [ ([i], [x])
      | (i, x) <- zip [0 ..] xs
    ]

main :: IO ()
main = do
  putStrLn "Table Operators:"
  let t1 :: Table Int Int
      t1 =
        fromList
          ['a', 'c']
          ['x', 'z']
          [ ([0, 0], [1, 2]),
            ([0, 1], [2, 4]),
            ([1, 0], [3, 6]),
            ([1, 1], [4, 8])
          ]

      t2 :: Table Int Int
      t2 =
        fromList
          ['c', 'b']
          ['z', 'y']
          [ ([0, 0], [1, 7]),
            ([0, 1], [3, 5]),
            ([1, 0], [5, 3]),
            ([1, 1], [7, 1])
          ]

      oplus :: Int -> Int -> Int
      oplus = max

      otimes :: Int -> Int -> Int
      otimes = (*)

      tUnion = tableUnion oplus t1 t2
      tStrict = strictJoin otimes t1 t2
      tRelaxed = relaxedJoin otimes t1 t2
      f _ _ = (['w'], Row [3])
      tExt = ext f t1

  putStrLn "t1:"
  putStr (prettyTable t1)
  putStrLn "t2:"
  putStr (prettyTable t2)
  putStrLn "tableUnion oplus t1 t2:"
  putStr (prettyTable tUnion)
  putStrLn "strictJoin otimes t1 t2:"
  putStr (prettyTable tStrict)
  putStrLn "relaxedJoin otimes t1 t2:"
  putStr (prettyTable tRelaxed)
  putStrLn "ext f t1:"
  putStr (prettyTable tExt)

  putStrLn ""
  putStrLn "Example 2:"
  let c = [0]
      a2 = [1, 2]
      f1 e = e <> c
      r2 = for 5 f1 a2
  putStr (prettyTable (vecTable r2))
  putStrLn "Example 3:"
  let a3 = [1, 2]
      b = [1]
      f2 e = fmap (* length e) e <> b
      cond e = sum e < 20
      step1 = a3
      step2 = f2 step1
      step3 = f2 step2
      r3 = iter f2 cond a3
  putStrLn "Step 1:"
  putStr (prettyTable (vecTable step1))
  putStrLn ("sum(A)=" <> show (sum step1))
  putStrLn "A := F2(Count(A),A):"
  putStr (prettyTable (vecTable step2))
  putStrLn "Step 2:"
  putStr (prettyTable (vecTable step2))
  putStrLn ("sum(A)=" <> show (sum step2))
  putStrLn "A := F2(Count(A),A):"
  putStr (prettyTable (vecTable step3))
  putStrLn "Step 3 (stop condition reached):"
  putStr (prettyTable (vecTable r3))
