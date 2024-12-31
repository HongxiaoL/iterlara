{-| IterLara: public re-export module.

This module contains no logic. It only re-exports the three core building blocks:

- "IterLara.Table": table representation and schemas
- "IterLara.Operators": table operators such as union/join/extend/map
- "IterLara.Iteration": generic iteration combinators (iter/for)
-}
module IterLara
  ( module IterLara.Table,
    module IterLara.Operators,
    module IterLara.Iteration
  )
where

import IterLara.Iteration
import IterLara.Operators
import IterLara.Table
