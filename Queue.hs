{-
---
fulltitle: "In class exercise: Purely Functional Queues"
date: October 10, 2022
---

Today's technical challenge is to implement a persistent *queue* data structure.
-}

module Queue where

{-
You should use QuickCheck to test this module. If you want to use additional
library operations to complete this exercise, you may import them here. (For
example, our solution uses at least one function from the `Data.Maybe`
library.)

-}

import Test.QuickCheck

{-
1. Define an *interface* for a purely functional Queue (FIFO).  Your interface
  must (at least) define some data structure, called `Q`, include a
  representation of an empty queue (queue), a way to add an element to the end
  of the queue (enq) and a way to remove the element at the beginning of the
  queue (deq), if the queue is nonempty. The queue must be polymorphic over
  the type of elements that it stores. You may include additional operations
  in your interface, if you wish.
-}

-- Use a list to implement a queue
data Q a = Q [a]
  deriving (Show, Eq)

empty :: Q a
-- enq returns the new larger queue
enq :: Q a -> a -> Q a
-- Different but isomorphic type signatures for deq
deq :: Q a -> (Maybe a, Q a)
-- deq' :: Q a -> Maybe (a, Q a)
-- deq'' :: Q a -> Maybe (Q a)
-- peek :: Q a -> Maybe a
{-
2. Now define some properties that your queue should satisfy. (Note: you may want
to add additional operations to your queue interface to help with stating
these properties.)
-}

-- Don't make your properties polymorphic! Need to make sure that they get tested with informative types
-- Polymorphic properties aren't that useful (GHCi will pick the unit type by default)
-- Having the unit type isn't very sueful since the unit type only has one element (so x becomes useless)

-- Checks that enqueueing and then dequeueing from the empty list yields the empty list
-- prop_EnqDeq :: Int -> Bool
prop_EnqDeq :: Int -> Q Int -> Bool
prop_EnqDeq x q = deq (enq q x) == (Just x, empty)

-- Checks that enqueueing a lot of items results in a queue impelmented as a list containing those items in order
-- Need to do foldl so that we enq in order
prop_EnqAlot :: [Int] -> Bool
prop_EnqAlot xs = foldl enq empty xs == Q xs

fromList :: [a] -> Q a
fromList = foldl enq empty

toList :: Q a -> [a]
toList (Q xs) = xs

{-
3. Implement your interface. Hint: the simplest implementation uses a single list. However, you can define a more efficient version using two lists.
-}

empty = Q []

enq (Q xs) x = Q (xs ++ [x])

deq (Q []) = (Nothing, Q [])
deq (Q (x : xs)) = (Just x, Q xs)

{-
4. Make an `Arbitrary` instance, including definitions for both `arbitrary` and `shrink`
-}

-- Note that arbitrary :: Gen [a] is a functor
-- Need to add the constraint that we can generate Arbitrary a
-- fmap takes a generator for [a] and turns it into a generator for Q a
instance Arbitrary a => Arbitrary (Q a) where
  arbitrary :: Gen (Q a)
  arbitrary = fmap Q (arbitrary :: Gen [a])

  shrink :: Q a -> [Q a]
  shrink (Q []) = []
  shrink (Q (x : xs)) = [Q xs]

{-
5. Run your tests.
-}
