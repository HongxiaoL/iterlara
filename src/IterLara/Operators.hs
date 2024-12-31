{-| Table operators (built on top of "Data.Map.Strict").

This module provides union/join/extend-like operators using explicit key/value schemas.
-}
module IterLara.Operators
  ( tableUnion,
    strictJoin,
    relaxedJoin,
    ext,
    mapTable
  )
where

import qualified Data.Map.Strict as M
import qualified Data.Set as S
import IterLara.Table (Name, Row (..), Schema, Table (..))

schemaCommon :: Schema -> Schema -> Schema
schemaCommon s1 s2 = filter (`elem` s2) s1

schemaUnion :: Schema -> Schema -> Schema
schemaUnion s1 s2 = s1 <> filter (`notElem` s1) s2

schemaIndex :: Schema -> M.Map Name Int
schemaIndex s = M.fromList (zip s [0 ..])

projectRow :: M.Map Name Int -> Schema -> Row a -> Row a
projectRow ix keep (Row xs) = Row [xs !! (ix M.! n) | n <- keep]

mergeRowWith :: (a -> a -> a) -> Row a -> Row a -> Row a
mergeRowWith f (Row xs) (Row ys) = Row (zipWith f xs ys)

mapRow :: (a -> b) -> Row a -> Row b
mapRow f (Row xs) = Row (fmap f xs)

mergeRowsBySchema :: (a -> a -> a) -> Schema -> Schema -> Row a -> Row a -> Schema -> Row a
mergeRowsBySchema f s1 s2 (Row xs1) (Row xs2) res =
  let ix1 = schemaIndex s1
      ix2 = schemaIndex s2
      get1 n = xs1 !! (ix1 M.! n)
      get2 n = xs2 !! (ix2 M.! n)
      both n = M.member n ix1 && M.member n ix2
      one1 n = M.member n ix1
   in Row
        [ if both n
            then f (get1 n) (get2 n)
            else
              if one1 n
                then get1 n
                else get2 n
          | n <- res
        ]

tableUnion :: (Ord k) => (v -> v -> v) -> Table k v -> Table k v -> Table k v
tableUnion oplus (Table ks1 vs1 r1) (Table ks2 vs2 r2) =
  let commonK = schemaCommon ks1 ks2
      resV = schemaUnion vs1 vs2
      ixK1 = schemaIndex ks1
      ixK2 = schemaIndex ks2
      agg1 =
        M.fromListWith
          (mergeRowWith oplus)
          [ (projectRow ixK1 commonK k, v)
            | (k, v) <- M.toList r1
          ]
      agg2 =
        M.fromListWith
          (mergeRowWith oplus)
          [ (projectRow ixK2 commonK k, v)
            | (k, v) <- M.toList r2
          ]
      commonKeys = S.intersection (M.keysSet agg1) (M.keysSet agg2)
      rs =
        M.fromList
          [ ( ck,
              mergeRowsBySchema oplus vs1 vs2 (agg1 M.! ck) (agg2 M.! ck) resV
            )
            | ck <- S.toList commonKeys
          ]
   in Table commonK resV rs

buildIndex :: (Ord k) => Schema -> Schema -> Table k v -> M.Map (Row k) [(Row k, Row v)]
buildIndex common ks (Table _ _ rs) =
  let ix = schemaIndex ks
   in foldr
        (\(k, v) acc -> M.insertWith (<>) (projectRow ix common k) [(k, v)] acc)
        M.empty
        (M.toList rs)

strictJoin :: (Ord k) => (v -> v -> v) -> Table k v -> Table k v -> Table k v
strictJoin otimes t1@(Table ks1 vs1 r1) t2@(Table ks2 vs2 _) =
  let commonK = schemaCommon ks1 ks2
      resK = schemaUnion ks1 ks2
      resV = schemaCommon vs1 vs2
      idx2 = buildIndex commonK ks2 t2
      ix1 = schemaIndex ks1
      ixV1 = schemaIndex vs1
      ixV2 = schemaIndex vs2
      pairs =
        [ ( mergeRowsBySchema (\a _ -> a) ks1 ks2 k1 k2 resK,
            mergeRowWith otimes (projectRow ixV1 resV v1) (projectRow ixV2 resV v2)
          )
          | (k1, v1) <- M.toList r1,
            let ck = projectRow ix1 commonK k1,
            (k2, v2) <- M.findWithDefault [] ck idx2
        ]
   in Table resK resV (M.fromList pairs)

relaxedJoin :: (Ord k) => (v -> v -> v) -> Table k v -> Table k v -> Table k v
relaxedJoin otimes t1@(Table ks1 vs1 r1) t2@(Table ks2 vs2 _) =
  let commonK = schemaCommon ks1 ks2
      resK = schemaUnion ks1 ks2
      resV = schemaUnion vs1 vs2
      idx2 = buildIndex commonK ks2 t2
      ix1 = schemaIndex ks1
      pairs =
        [ ( mergeRowsBySchema (\a _ -> a) ks1 ks2 k1 k2 resK,
            mergeRowsBySchema otimes vs1 vs2 v1 v2 resV
          )
          | (k1, v1) <- M.toList r1,
            let ck = projectRow ix1 commonK k1,
            (k2, v2) <- M.findWithDefault [] ck idx2
        ]
   in Table resK resV (M.fromList pairs)

ext :: (Row k -> Row v -> (Schema, Row v')) -> Table k v -> Table k v'
ext f (Table ks _ rs) =
  let newVs =
        case M.lookupMin rs of
          Nothing -> fst (f (Row []) (Row []))
          Just (k, v) -> fst (f k v)
   in Table ks newVs (M.mapWithKey (\k v -> snd (f k v)) rs)

mapTable :: (v -> v') -> Table k v -> Table k v'
mapTable f (Table ks vs rs) = Table ks vs (fmap (mapRow f) rs)
