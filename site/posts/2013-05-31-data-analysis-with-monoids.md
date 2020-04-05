---
title: Data analysis with Monoids
author: Tim Docker
date: 2013-05-31
tags: [haskell]
---
This post expresses the key ideas of a talk I gave at FP-SYD this week.

[Monoids](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-Monoid.html)
are a pretty simple concept in haskell. Some years ago I learnt of them
through the excellent
[Typeclassopedia](http://www.haskell.org/haskellwiki/Typeclassopedia),
looked at the examples, and understood them quickly (which is more than
can be said for many of the new ideas that one learns in haskell).
However that was it. Having learnt the idea, I realised that monoids are
everywhere in programming, but I'd not found much use for the Monoid
typeclass abstraction itself. Recently, I've found they can be a useful
tool for data analysis...

Monoids
-------

First a quick recap. A monoid is a type with a binary operation, and an
identity element:

    class Monoid a where
      mempty :: a
      mappend :: a -> a -> a

It must satisfy a simple set of laws, specifically that the binary
operation much be associative, and the identity element must actually be
the identity for the given operation:

    mappend a (mappend b c) = mappend (mappend a b) c
    mappend mempty x = x
    mappend x mempty = x

As is hinted by the names of the typeclass functions, lists are an
obvious Monoid instance:

    instance Monoid [a] where
      mempty  = []
      mappend = (++)

However, many types can be Monoids. In fact, often a type can be a
monoid in multiple ways. Numbers are monoids under both addition and
multiplication, with 0 and 1 as their respective identity elements. In
the haskell standard libraries, rather than choose one kind of monoid
for numbers, newtype declarations are used to given instances for both:

    newtype Sum a = Sum { getSum :: a }
      deriving (Eq, Ord, Read, Show, Bounded)

    instance Num a => Monoid (Sum a) where
      mempty = Sum 0
      Sum x `mappend` Sum y = Sum (x + y)

    newtype Product a = Product { getProduct :: a }
      deriving (Eq, Ord, Read, Show, Bounded)

    instance Num a => Monoid (Product a) where
      mempty = Product 1
      Product x `mappend` Product y = Product (x * y)

We've now established and codified the common structure for a few
monoids, but it's not yet clear what it has gained us. The Sum and
Product instances are unwieldly - you are unlikely to want to use Sum
directly to add two numbers:

    Prelude> :m Data.Monoid
    Prelude Data.Monoid> 5+4
    9
    Prelude Data.Monoid> getSum (mappend (Sum 5) (Sum 4))
    9

Before we progress, however, let's define a few more monoid instances,
potentially useful for data analysis.

    data Min a = Min a | MinEmpty deriving (Show)
               
    data Max a = Max a | MaxEmpty deriving (Show)

    newtype Count = Count Int deriving (Show)

    instance (Ord a) => Monoid (Min a) where
      mempty = MinEmpty
      mappend MinEmpty m = m
      mappend m MinEmpty = m
      mappend (Min a) (Min b) = (Min (P.min a b))

    instance (Ord a) => Monoid (Max a) where
      mempty = MaxEmpty
      mappend MaxEmpty m = m
      mappend m MaxEmpty = m
      mappend (Max a) (Max b) = (Max (P.max a b))

    instance Monoid Count where
      mempty = Count 0
      mappend (Count n1) (Count n2) = Count (n1+n2)

Also some helper functions to construct values of all these monoid
types:

    sum :: (Num a) => a -> Sum a
    sum = Sum

    product :: (Num a) => a -> Product a
    product = Product

    min :: (Ord a) => a -> Min a
    min = Min

    max :: (Ord a) => a -> Max a
    max = Max

    count :: a -> Count
    count _ = Count 1

These functions are trivial, but they put a consistent interface on
creating monoid values. They all have a signature (a -&gt; m) where m is
some monoid. For lack of a better name, I'll call functions with such
signatures "monoid functions".

Foldable
--------

It's time to introduce another typeclass,
[Foldable](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Data-Foldable.html).
This class abstracts the classic foldr and foldl functions away from
lists, making them applicable to arbitrary structures. (There's a robust
debate going on right now about the merits of replacing the list
specific fold functions in the standard prelude with the more general
versions from Foldable.) Foldable is a large typeclass - here's the key
function of interest to us:

    class Foldable t where
      ...
      foldMap :: Monoid m => (a -> m) -> t a -> m
      ...

foldMap takes a monoid function and a Foldable structure, and reduces
the structure down to a single value of the monoid. Lists are, of
course, instances of foldable, so we can demo our helper functions:

    *Examples> let as = [45,23,78,10,11,1]
    *Examples> foldMap count as
    Count 6
    *Examples> foldMap sum as
    Sum {getSum = 168}
    *Examples> foldMap max as
    Max 78

Notice how the results are all still wrapped with the newtype
constructors. We'll deal with this later.

Composition
-----------

As it turns out, tuples are already instances of Monoids:

    instance (Monoid a,Monoid b) => Monoid (a,b) where
      mempty = (mempty,mempty)
      mappend (a1,b1) (a2,b2) = (mappend a1 a2,mappend b1 b2)

A pair is a monoid if it's elements are monoids. There are similar
instances for longer tuples. We need some helper monoid functions for
tuples also:

    a2 :: (a -> b) -> (a -> c) -> a -> (b,c)
    a2 b c = (,) <$> b <*> c

    a3 :: (a -> b) -> (a -> c) -> (a -> d) -> a -> (b,c,d)
    a3 b c d = (,,) <$> b <*> c <*> d

These are implemented above using
[Applicative](http://www.haskell.org/ghc/docs/latest/html/libraries/base/Control-Applicative.html)
operators, though I've given them more restrictive types to make their
intended use here clearer. Now I can compose monoid functions:

    *Examples> let as = [45,23,78,10,11,1]
    *Examples> :t (a2 min max)
    (a2 min max) :: Ord a => a -> (Min a, Max a)
    *Examples> foldMap (a2 min max) as
    (Min 1,Max 78)
    *Examples> :t (a3 count (a2 min max) (a2 sum product))
    (a3 count (a2 min max) (a2 sum product))
      :: (Num a, Ord a) =>
         a -> (Count, (Min a, Max a), (Sum a, Product a))
    *Examples> foldMap (a3 count (a2 min max) (a2 sum product)) as
    (Count 6,(Min 1,Max 78),(Sum {getSum = 168},Product {getProduct = 8880300}))

It's worth noting here that the composite computations are done in a
single traversal of the input list.

More complex calculations
-------------------------

Happy with this, I decide to extend my set of basic computations with
the arithmetic mean. There is a problem, however. The arithmetic mean
doesn't "fit" as a monoid - there's no binary operation such that a mean
for a combined set of data can be calculated from the mean of two
subsets.

What to do? Well, the mean is the sum divided by the count, both of
which are monoids:

    newtype Mean a = Mean (Sum a,Count) deriving (Show)

    instance (Num a) => Monoid (Mean a) where
      mempty = Mean mempty
      mappend (Mean m1) (Mean m2) = Mean (mappend m1 m2)

    mean v = Mean (Sum v,Count 1)

So I can calculate the mean if I am prepared to do a calculation after
the foldMap:

    *Examples> let as = [45,23,78,10,11,1.5]
    *Examples> foldMap mean as
    Mean (Sum {getSum = 168.5},Count 6)
    *Examples> let (Mean (Sum t,Count n)) = foldMap mean as in t / fromIntegral n
    28.083333333333332

The Aggregation type class
--------------------------

For calculations like `mean`, I need something more than a monoid. I
need a monoid for accumulating the values, and then, once the
accumulation is complete, a postprocessing function to compute the final
result. Hence a new typeclass to extend Monoid:

    {-# LANGUAGE TypeFamilies #-}

    class (Monoid a) => Aggregation a where
      type AggResult a :: *
      aggResult :: a -> AggResult a

This makes use of the [type families ghc
extension](http://www.haskell.org/haskellwiki/GHC/Type_families). We
need this to express the fact that our postprocessing function aggResult
has a different return type to the type of the monoid. In the above
definition:

-   aggResult is a function that gives you the *value* of the final
    result from the *value* of the monoid
-   AggResult is a *type* function that gives you the *type* of the
    final result from the *type* of the monoid

We can write an instance of Aggregation for Mean:

    instance (Fractional a) => Aggregation (Mean a) where
      type AggResult (Mean a) = a
      aggResult (Mean (Sum t,Count n)) = t/fromIntegral n

and test it out:

    *Examples> let as = [45,23,78,10,11,1.5]
    *Examples> aggResult (foldMap mean as)
    28.083333333333332
    *Examples> 

Nice. Given that `aggResult (foldMap ...)` will be a common pattern,
lets write a helper:

    afoldMap :: (Foldable t, Aggregation a) => (v -> a) -> t v -> AggResult a
    afoldMap f vs = aggResult (foldMap f vs)

In order to use the monoids we defined before (sum,product etc) we need
to define Aggregation instances for them also. Even though they are
trivial, it turns out to be useful, as we can make the aggResult
function strip off the newtype constructors that were put there to
enable the Monoid typeclass:

    instance (Num a) => Aggregation (Sum a)  where
      type AggResult (Sum a) = a
      aggResult (Sum a) = a
        
    instance (Num a) => Aggregation (Product a)  where
      type AggResult (Product a) = a
      aggResult (Product a) = a

    instance (Ord a) => Aggregation (Min a)  where
      type AggResult (Min a) = a
      aggResult (Min a) = a

    instance (Ord a) => Aggregation (Max a)  where
      type AggResult (Max a) = a
      aggResult (Max a) = a

    instance Aggregation Count where
      type AggResult Count = Int
      aggResult (Count n) = n

    instance (Aggregation a, Aggregation b) => Aggregation (a,b) where
      type AggResult (a,b) = (AggResult a, AggResult b)
      aggResult (a,b) = (aggResult a, aggResult b)

    instance (Aggregation a, Aggregation b, Aggregation c) => Aggregation (a,b,c) where
      type AggResult (a,b,c) = (AggResult a, AggResult b, AggResult c)
      aggResult (a,b,c) = (aggResult a, aggResult b, aggResult c)

This is mostly boilerplate, though notice how the tuple instances delve
into their components in order to postprocess the results. Now
everything fits together cleanly:

    *Examples> let as = [45,23,78,10,11,1.5]
    *Examples> :t (a3 count (a2 min max) mean)
    (a3 count (a2 min max) mean)
      :: Ord a => a -> (Count, (Min a, Max a), Mean a)
    *Examples> afoldMap (a3 count (a2 min max) mean) as
    (6,(1.5,78.0),28.083333333333332)
    *Examples> 

The 4 computations have been calculated all in a single pass over the
input list, and the results are free of the type constructors that are
no longer required once the aggregation is complete.

Another example of an Aggregation where we need to postprocess the
result is counting the number of unique items. For this we will keep a
set of the items seen, and then return the size of this set at the end:

    newtype CountUnique a = CountUnique (Set.Set a)

    instance Ord a => Monoid (CountUnique a) where
      mempty = CountUnique Set.empty
      mappend (CountUnique s1) (CountUnique s2) = CountUnique (Set.union s1 s2)

    instance Ord a => Aggregation (CountUnique a) where
      type AggResult (CountUnique a) = Int
      aggResult (CountUnique s1) = Set.size s1

    countUnique :: Ord a => a -> CountUnique a
    countUnique a = CountUnique (Set.singleton a)

.. in use:

    *Examples> let as = [5,7,8,7,11,10,11]
    *Examples> afoldMap (a2 countUnique count) as
    (5,7)

Higher order aggregation functions
----------------------------------

All of the calculations seen so far have worked consistently across all
values in the source data structure. We can make use of the `mempty`
monoid value in order to filter our data set, and or aggregate in
groups. Here's a couple of higher order monoid functions for this:

    afilter :: Aggregation m => (a -> Bool) -> (a -> m) -> (a -> m)
    afilter match mf = \a -> if match a then mf a else mempty

    newtype MMap k v = MMap (Map.Map k v)
      deriving Show

    instance (Ord k, Monoid v) => Monoid (MMap k v) where
      mempty = MMap (Map.empty)
      mappend (MMap m1) (MMap m2) = MMap (Map.unionWith mappend m1 m2)

    instance (Ord k, Aggregation v) => Aggregation (MMap k v) where
      type AggResult (MMap k v) = Map.Map k (AggResult v)
      aggResult (MMap m) = Map.map aggResult m

    groupBy :: (Ord k, Aggregation m) => (a -> k) -> (a -> m) -> (a -> MMap k m)
    groupBy keyf valuef = \a -> MMap (Map.singleton (keyf a) (valuef a))

`afilter` restricts the application of a monoid function to a subset of
the input data. eg to calculate the sum of all the values, and the sum
of values less than 20:

    *Examples> let as = [5,10,20,45.4,35,1,3.4]
    *Examples> afoldMap (a2 sum (afilter (<=20) sum)) as
    (119.8,39.4)

`groupBy` takes a key function and a monoid function. It partitions the
data set using the key function, and applies a monoid function to each
subset, returning all of the results in a map. Non-numeric data works
better as an example here. Let's take a set of words as input, and for
each starting letter, calculate the number of words with that letter,
the length of the shortest word, and and the length of longest word:

    *Examples> let as = words "monoids are a pretty simple concept in haskell some years ago i learnt of them through the excellent typeclassopedia looked at the examples and understood them straight away which is more than can be said for many of the new ideas that one learns in haskell"
    *Examples> :t groupBy head (a3 count (min.length) (max.length))
    groupBy head (a3 count (min.length) (max.length))
      :: Ord k => [k] -> MMap k (Count, Min Int, Max Int)
    *Examples> afoldMap (groupBy head (a3 count (min.length) (max.length))) as
    fromList [('a',(6,1,4)),('b',(1,2,2)),('c',(2,3,7)),('e',(2,8,9)),('f',(1,3,3)),('h',(2,7,7)),('i',(5,1,5)),('l',(3,6,6)),('m',(3,4,7)),('n',(1,3,3)),('o',(3,2,3)),('p',(1,6,6)),('s',(4,4,8)),('t',(9,3,15)),('u',(1,10,10)),('w',(1,5,5)),('y',(1,5,5))]

Many useful data analysis functions can be written through simple
function application and composition using these primitive monoid
functions, the product combinators a2 and a3 and these new filtering and
grouping combinators.

Disk-based data
---------------

As pointed out before, regardless of the complexity of the computation,
it's done with a single traversal of the input data. This means that we
don't need to limit ourselves to lists and other in memory Foldable data
structures. Here's a function similar to foldMap, but that works over
the lines in a file:

    foldFile :: Monoid m => FilePath -> (BS.ByteString -> Maybe a) -> (a -> m) -> IO m
    foldFile fpath pf mf = do
      h <- openFile fpath ReadMode
      m <- loop h mempty
      return m
      where
        loop h m = do
          eof <- hIsEOF h
          if eof
            then (return m)
            else do
              l <- BS.hGetLine h
              case pf l of
                Nothing -> loop h m
                (Just a) -> let m' = mappend m (mf a)
                            in loop h m'

    afoldFile :: Aggregation m => FilePath -> (BS.ByteString -> Maybe a) -> (a -> m) -> IO (AggResult m)
    afoldFile fpath pf mf = fmap aggResult (foldFile fpath pf mf)

foldFile take two parameters - a function to parse each line of the
file, the other is the monoid function to do the aggregation. Lines that
fail to parse are skipped. (I can here questions in the background "What
about strictness and space leaks?? - I'll come back to that). As an
example usage of aFoldFile, I'll analyse some stock data. Assume that I
have it in a CSV file, and I've got a function to parse one CSV line
into a sensible data value:

    import qualified Data.ByteString.Char8 as BS
    import Data.Time.Calendar

    data Prices = Prices {
      pName :: String,          -- The stock code
      pDate :: Day,             -- The historic date
      pOpen :: Double,          -- The price at market open
      pHigh :: Double,          -- The highest price on the date
      pLow :: Double,           -- The lowest price on the date
      pClose :: Double,         -- The price at market close
      pVolume :: Double         -- How many shares were traded
      } deriving (Show)

      parsePrices :: BS.ByteString -> Maybe Prices
      parsePrices = ...

Now I can use my monoid functions to analyse the file based data. How
many google prices do I have, over what date range:

    *Examples> let stats =  afilter (("GOOG"==).pName) (a3 count (min.pDate) (max.pDate))
    *Examples> :t stats
    stats
      :: Prices
         -> (Count,
             Min time-1.4:Data.Time.Calendar.Days.Day,
             Max time-1.4:Data.Time.Calendar.Days.Day)
    *Examples> afoldFile "prices.csv" parsePrices stats
    (1257,2008-05-29,2013-05-24)
    *Examples> 

Perhaps I want to aggregate my data per month, getting traded price
range and total volume. We need a helper function to work out the month
of each date:

    startOfMonth :: Day -> Day
    startOfMonth t = let (y,m,d) = toGregorian t
                     in fromGregorian y m 1

And then we can use groupBy to collect data monthly:

    :*Examples> let stats =  afilter (("GOOG"==).pName) (groupBy (startOfMonth.pDate) (a3 (min.pLow) (max.pHigh) (sum.pVolume)))
    *Examples> :t stats
    stats
      :: Prices
         -> MMap
              time-1.4:Data.Time.Calendar.Days.Day
              (Min Double, Max Double, Sum Double)
    *Examples> results <- afoldFile "prices.csv" parsePrices stats
    *Examples> mapM_ print (Map.toList results)
    (2008-05-01,(573.2,589.92,8073107.0))
    (2008-06-01,(515.09,588.04,9.3842716e7))
    (2008-07-01,(465.6,555.68,1.04137619e8))
    ...
    (2013-03-01,(793.3,844.0,4.2559856e7))
    (2013-04-01,(761.26,827.64,5.3574633e7))
    (2013-05-01,(816.36,920.6,4.1080028e7))

Conclusion
----------

So, I hope I've shown that monoids are useful indeed. They can form the
core of a framework for cleanly specifing quite complex data analysis
tasks.

An additional typeclass which I called "Aggregation" extends Monoid and
provides for a broader range of computations and also cleaner result
types (thanks to type families). There was some discussion when I
presented this talk as to whether a single method typeclass like
Aggregation was a "true" abstraction, given it has no associated laws.
This is a valid point, however using it simplifies the syntax and usage
of monoidal calculations significantly, and for me, this makes it worth
having.

There remains an elephant in the room, however, and this is space
leakage. Lazy evalulation means that, as written, most of the
calculations shown run in space proportional to the input data set.
Appropriate strictness annotations and related modifications will fix
this, but it turns out to be slightly irritating. This blog post is
already long enough, so I'll address space leaks in in a subsequent
post...
