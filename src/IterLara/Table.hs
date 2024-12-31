{-| Minimal table definitions.

Tables are backed by "Data.Map.Strict" and carry explicit schemas:

- 'Schema': a list of field names (chars)
- 'Row': a row whose ordering follows the schema
- 'Table': a map from key rows to value rows, with separate key/value schemas
-}
module IterLara.Table
  ( Name,
    Schema,
    Row (..),
    row,
    Table (..),
    fromList,
    tableKeySchema,
    tableValSchema,
    ExprMap,
    emptyTable
  )
where

import qualified Data.Map.Strict as M
import qualified Data.Set as S

type Name = Char

type Schema = [Name]

newtype Row a = Row {unRow :: [a]}
  deriving (Eq, Ord, Show)

row :: [a] -> Row a
row = Row

data Table k v = Table
  { keySchema :: Schema,
    valSchema :: Schema,
    tableRows :: M.Map (Row k) (Row v)
  }
  deriving (Eq, Show)

fromList :: (Ord k) => Schema -> Schema -> [([k], [v])] -> Table k v
fromList ks vs =
  Table ks vs
    . M.fromList
    . fmap (\(k, v) -> (Row k, Row v))

-- | A table transformation, suitable to be used with 'IterLara.Iteration.iter'/'for'.
type ExprMap k v = Table k v -> Table k v

-- | Empty table with a fixed schema.
emptyTable :: Schema -> Schema -> Table k v
emptyTable ks vs = Table ks vs M.empty

tableKeySchema :: Table k v -> S.Set Name
tableKeySchema = S.fromList . keySchema

tableValSchema :: Table k v -> S.Set Name
tableValSchema = S.fromList . valSchema
