# type-indexed-queues
## Queues and Heaps with verified and unverified versions.

[![Build Status](https://travis-ci.org/oisdk/type-indexed-queues.svg?branch=master)](https://travis-ci.org/oisdk/type-indexed-queues)



This library provides implementations of five different heaps
(binomial, pairing, skew, leftist, and Braun), each in two
flavours: one verified, and one not.

At the moment, only structural invariants are maintained.

## Comparisons of verified and unverified heaps
Both versions of each heap are provided for comparison: for
instance, compare the standard leftist heap (in
Data.Heap.Leftist):

```haskell
data Leftist a
  = Leaf
  | Node !Int
        a
        (Leftist a)
        (Leftist a)
```

To its size-indexed counterpart (in Data.Heap.Indexed.Leftist):

```haskell
data Leftist n a where
        Leaf :: Leftist 0 a
        Node :: !(The Nat (n + m + 1))
             -> a
             -> Leftist n a
             -> Leftist m a
             -> !(m <= n)
             -> Leftist (n + m + 1) a
```

The invariant here (that the size of the left heap must
always be less than that of the right) is encoded in the
proof `m <= n`.
 
With that in mind, compare the unverified and verified
implementatons of `merge`:

```haskell
merge Leaf h2 = h2
merge h1 Leaf = h1
merge h1@(Node w1 p1 l1 r1) h2@(Node w2 p2 l2 r2)
  | p1 < p2 =
      if ll <= lr
          then Node (w1 + w2) p1 l1 (merge r1 h2)
          else Node (w1 + w2) p1 (merge r1 h2) l1
  | otherwise =
      if rl <= rr
          then Node (w1 + w2) p2 l2 (merge r2 h1)
          else Node (w1 + w2) p2 (merge r2 h1) l2
  where
    ll = rank r1 + w2
    lr = rank l1
    rl = rank r2 + w1
    rr = rank l2
```

Verified:

```haskell
merge Leaf h2 = h2
merge h1 Leaf = h1
merge h1@(Node w1 p1 l1 r1 _) h2@(Node w2 p2 l2 r2 _)
  | p1 < p2 =
      if ll <=. lr
        then Node (w1 +. w2) p1 l1 (merge r1 h2)
        else Node (w1 +. w2) p1 (merge r1 h2) l1 . totalOrder ll lr
  | otherwise =
      if rl <=. rr
          then Node (w1 +. w2) p2 l2 (merge r2 h1)
          else Node (w1 +. w2) p2 (merge r2 h1) l2 . totalOrder rl rr
  where
    ll = rank r1 +. w2
    lr = rank l1
    rl = rank r2 +. w1
    rr = rank l2
```

## Using type families and typechecker plugins to encode the invariants
The similarity is accomplished through overloading, and some
handy functions. For instance, the second if-then-else works
on boolean *singletons*, and the `<=.` function provides a
proof of order along with its answer. The actual arithmetic
is carried out at runtime on normal integers, rather than
Peano numerals. These tricks are explained in more detail
TypeLevel.Singletons and TypeLevel.Bool.

A typechecker plugin does most of the heavy lifting, although
there are some (small) manual proofs.

## Uses of verified heaps
The main interesting use of these sturctures is total traversable
sorting ([sort-traversable](https://github.com/treeowl/sort-traversable)).
An implementation of this is provided in Data.Traversable.Sort. I'm
interested in finding out other uses for these kinds of structures.
