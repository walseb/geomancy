module Geomancy.Tree
  ( Tree(..)

  , apply
  , applyWith
  , mapAccum

  , node_
  , leaf_
  , collect_

  , annotateMap
  , annotateWith
  ) where

import Data.Tree (Tree(..))
import Data.Foldable (toList)

-- * Merging annotations

{- |
  Distribute annotations down the tree without changing the type.
-}
{-# INLINEABLE apply #-}
apply :: Semigroup ann => Tree (ann, a) -> Tree (ann, a)
apply (Node (rootAnn, root) rootChildren) =
  Node
    (rootAnn, root)
    (map (applyWith (<>) rootAnn) rootChildren)

{- |
  Distribute accumulator down the tree using the accumulator function.
-}
{-# INLINEABLE applyWith #-}
applyWith
  :: (ann -> acc -> acc)
  -> acc
  -> Tree (ann, a)
  -> Tree (acc, a)
applyWith f = mapAccum next
  where
    -- nextAcc = f ann acc
    next acc (ann, item) =
      let
        acc' = f ann acc
      in
        (acc', (acc', item))

{- |
  Transform a tree by combining branch-independent accumulator with node contents.
-}
{-# INLINEABLE mapAccum #-}
mapAccum
  :: (t -> a -> (t, b))
  -> t
  -> Tree a
  -> Tree b
mapAccum f acc (Node item children) =
  Node
    nextNode
    (map (mapAccum f nextAcc) children)
  where
    (nextAcc, nextNode) = f acc item

-- ** Shortcuts for monoidal annotation and Maybe-wrapped items

{-# INLINEABLE node_ #-}
node_ :: ann -> [Tree (ann, Maybe a)] -> Tree (ann, Maybe a)
node_ ann = Node (ann, Nothing)

{-# INLINEABLE leaf_ #-}
leaf_ :: Monoid ann => a -> Tree (ann, Maybe a)
leaf_ x = Node (mempty, Just x) []

collect_ :: Monoid ann => Tree (ann, Maybe a) -> [(ann, a)]
collect_ root = do
  (ann, Just item) <- toList $ apply root
  pure (ann, item)

-- * Adding annotations

{- |
  Annotate nodes with bottom-up monoidal summary.
-}
annotateMap
  :: Monoid ann
  => (a -> ann)
  -> Tree a
  -> Tree (ann, a)
annotateMap f =
  annotateWith f (\x anns -> f x <> mconcat anns)

{- |
  Annotate the nodes with bottom-up summary.
-}
annotateWith
  :: (a -> ann)
  -> (a -> [ann] -> ann)
  -> Tree a
  -> Tree (ann, a)
annotateWith leaf node = go
  where
    go (Node x ts) =
      case ts of
        [] ->
          Node (leaf x, x) []
        _ ->
          let
            inner = map go ts
          in
            Node
              ( node
                  x
                  (map (fst . rootLabel) inner)
              , x
              )
              inner
