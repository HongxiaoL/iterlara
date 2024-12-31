{-| Iteration combinators.

This module provides small, generic iteration utilities:

- 'iter': repeat until a stop condition becomes false
- 'for': repeat a fixed number of times
- 'countIter': compute the number of iterations from the current state
-}
module IterLara.Iteration
  ( iter,
    for,
    countIter
  )
where

-- | Repeatedly apply @f@ while @cond@ holds; return the first state where @cond@ is false.
--
iter :: (a -> a) -> (a -> Bool) -> a -> a
iter f cond t =
  if cond t
    then iter f cond (f t)
    else t

for :: Int -> (a -> a) -> a -> a
for n f t
  | n <= 0 = t
  | otherwise = for (n - 1) f (f t)

-- | Compute the number of iterations from the current state, then run 'for'.
countIter :: (a -> a) -> (a -> Int) -> a -> a
countIter f nFun t = for (nFun t) f t
