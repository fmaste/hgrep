
module Control.Arrow.Stream (
	Stream,
	put,
	get,
	runStream,
	puts,
	skip) where

-------------------------------------------------------------------------------

import Prelude hiding (id, (.)) -- Using id and . from Category
import Control.Category
import Control.Arrow
import Data.Sequence
import Data.Either

-------------------------------------------------------------------------------

-- One way to represent stream processors is using the datatype:
data Stream i o = Put o (Stream i o) | Get (i -> Stream i o)
-- where Put o f represents a stream processor that is ready to output o and
-- continue with f, and Get k represents a stream processor waiting for an
-- input i, which will continue by passing it to k.
-- Stream processors are them constructed using the following continuation 
-- style operations for these actions:
put :: o -> Stream i o -> Stream i o
put = Put
-- which constructs a stream processor which outputs the o and them behaves
-- like the second argument, and
get :: (i -> Stream i o) -> Stream i o
get = Get
-- which constructs a stream processor which waits for an input, passes it
-- to its function argument, and them behaves like the result.
-- Stream processors can be interpreted as stream functions by the function:
runStream :: Stream i o -> [i] -> [o]
runStream (Put o s) is     = o : runStream s is
runStream (Get k)   (i:is) = runStream (k i) is
runStream (Get k)   []     = []
-- We concern ourselfs for the time being with processes that have one input
-- channel and one output channel. 
-- For simplicity we shall only consider non-terminating (recursively defined) 
-- stream processors; otherwise we would add another operator to construct a
-- stream processor which halts.

-------------------------------------------------------------------------------

instance Category Stream where

	-- id :: cat a a
	-- Redirects the whole stream to the output untouched.
	-- Get something them put it back and continue doing the same.
	id = get $ \i -> put i id

	-- (.) :: cat d c -> cat b d -> cat b c
	-- Serial composition of stream processes.
	-- The input of the first parameter is the output of the second one.
	(Put o s) . stream = put o (s . stream)
	(Get f) . (Put o s) = (f o) . s
	(Get f) . (Get g) = get $ \i -> (get f) . (g i)

-- Defined by Control.Arrow:

-- (>>>) :: a b d -> a d c -> a b c
-- f >>> g = g . f

-- (<<<) :: a d c -> a b d -> a b c
-- f <<< g = f . g

-------------------------------------------------------------------------------

instance Arrow Stream where

	-- arr :: (b -> c) -> a b c
	-- Builds a stateless process that just applies a given function to its 
	-- input to produce its outputs. Also called mapStream.
	arr f = get $ \i -> put (f i) (arr f)

	-- first :: a b c -> a (b, d) (c, d)
	-- Builds a process that feeds the first components of its inputs
	-- through its argument process, while the second components bypass 
	-- the process and are recombined with its outputs.
	first s = bypass empty s where
		bypass q (Get f) = get $ \(i, d) -> bypass (q |> d) (f i)
		bypass q (Put o s) = 
			case viewl q of
				EmptyL -> get $ \(b, d) -> put (o, d) (bypass empty s)
				d :< q' -> put (o, d) (bypass q' s)
	-- runStream (first (get $ \b -> id)) [(1, 'a'), (2, 'b')]  :=:  [(2,'a')]
	-- runStream (first (put 0 id)) [(1, 'a'), (2, 'b')]  :=:  [(0,'a'),(2,'b')]
	-- runStream (first (get $ \b -> put 0 id)) [(1, 'a'), (2, 'b')]  :=:  [(0,'a'),(2,'b')]
	-- runStream (first (put 0 $ get $ \b -> id)) [(1, 'a'), (2, 'b')]  :=:  [(0,'a')]

	-- second :: a b c -> a (d, b) (d, c)
	-- Using default: 
	-- arr swap >>> first f >>> arr swap
	--      where   swap :: (x,y) -> (y,x)
	--              swap ~(x,y) = (y,x)

	-- (***) :: a b c -> a b' c' -> a (b, b') (c, c')
	-- Using default: 
	-- f *** g = first f >>> second g

	-- (&&&) :: a b c -> a b c' -> a b (c, c')
	-- Using default: 
	-- f &&& g = arr (\b -> (b,b)) >>> f *** g

-------------------------------------------------------------------------------

-- Arrow laws:

-- arr Prelude.id >>> f = f
-- f >>> arr Prelude.id = f

-- (f >>> g) >>> h = f >>> (g >>> h)
-- arr (g Prelude.. f) = arr f >>> arr g

-- first (arr f) = arr (\(a,b) -> (f a,b))
-- first (f >>> g) = first f >>> first g
-- first f >>> arr (\(a,b) -> (a,g b)) = arr (\(a,b) -> (a,g b)) >>> first f
-- first f >>> arr fst = arr fst >>> f
-- first (first f) >>> arr assoc = arr assoc >>> first f
-- where:
-- assoc :: ((a, b), c) -> (a, (b, c))
-- assoc ((a, b), c) = (a, (b, c))

-------------------------------------------------------------------------------

-- Monoid operations:

instance ArrowZero Stream where

	-- zeroArrow :: a b c
	-- The /dev/null stream processor.
	-- Stream processor also support the natural notion of failure:
	-- a failing process simply consumes all input and never produces 
	-- more output.
	zeroArrow = get $ \_ -> zeroArrow

instance ArrowPlus Stream where

	-- (<+>) :: a b c -> a b c -> a b c
	-- Parallel composition of stream processes.
	-- We define p <+> q to run in parallel, merging their outputs.
	-- All the Puts are done prioritizing p, when no more Puts are available
	-- an input is read and given to p and them to q in parallel.
        (Put o s) <+> s' = put o (s <+> s')
	s <+> (Put o s') = put o (s <+> s') 
	(Get f) <+> (Get g) = get $ \i -> (f i) <+> (g i)

-- This definitions satisfy this monoidal laws:
-- zeroArrow <+> q = q
-- p <+> zeroArrow = p
-- (p <+> q) <+> r = p <+> (q <+> r)
-- And also this choice and failure laws:
-- zeroArrow . q = zeroArrow (The same as zeroArrow <<< q or q >>> zeroArrow)
-- p . zeroArrow = zeroArrow (The same as p <<< zeroArrow or zeroArrow >>> p)

-------------------------------------------------------------------------------

-- Stream processors can also support dynamic choice.
-- f ||| g can be seen as another kind of parellel composition. 
-- This class underlies the if and case constructs in arrow notation. 

instance ArrowChoice Stream where

	-- left :: a b c -> a (Either b d) (Either c d)
	-- Converts the process on one that passes messages tagged Left 
	-- through it while the Right ones are passed untouched.
	left (Put o s) = put (Left o) (left s)
	left (Get f) = get $ either 
				(\i -> left (f i)) 
				(\d -> put (Right d) (left $ get f))

	-- right :: a b c -> a (Either d b) (Either d c)
	-- A mirror image of 'left'.
	-- Using default:
	-- right f = arr mirror >>> left f >>> arr mirror
	-- 	where	mirror :: Either x y -> Either y x
	--		mirror (Left x) = Right x
	--		mirror (Right y) = Left y

	-- (+++) :: a b c -> a b' c' -> a (Either b b') (Either c c')
	-- Split the input between the two argument arrows, retagging and
	-- merging their outputs.
	-- Using default:
	-- f +++ g = left f >>> right g

	-- (|||) :: a b d -> a b' d -> a (Either b b') d
	-- Fanin: Split the input between the two argument arrows and merge
	-- their outputs.
	-- Usign default:
	-- f ||| g = f +++ g >>> arr untag
	--	where	untag (Left x) = x
	--		untag (Right y) = y

-- With these definitions, then f ||| g can be regarded as yet another kind of
-- parellel composition, which routes inputs tagged Left to f and inputs tagged
-- Right to g.
-- In fact, although stream processors have only one input and one output 
-- channel, we can model processes with many of each by multiplexing several
-- channeld onto one. For example, we can regard a channel carrying messages 
-- of type Either a b as a representation for two channels, one carrying as and 
-- the other carrying bs. With this viewpoint, f ||| g combines f and g in 
-- parallel to yield a stream processor with two input channels (multiplexed 
-- onto one), and merges the output channeld onto one. 

-------------------------------------------------------------------------------

{-
instance ArrowLoop Stream where

	-- loop :: a (b, d) (c, d) -> a b c
	loop (Put (c,d) s) = 
	loop (Get f) = get $ \
-}

delay :: i -> Stream i i
delay i = put i id

-------------------------------------------------------------------------------

-- Stream utility functions:

-- Continuation passing style put for many elements.
puts :: [o] -> Stream i o -> Stream i o
puts [] s = s
puts (o:os) s = put o (puts os s)

-- Continuation passing style skip function.
skip :: Integer -> Stream i o -> Stream i o
skip n s
	| n > 0 = get $ \i -> skip (n - 1) s
	| otherwise = s

devNull :: Stream b c
devNull = zeroArrow

constStream :: o -> Stream i o
constStream o = get $ \b -> put o (constStream o)

filterStream :: (i -> Bool) -> Stream i i
filterStream f = get $ \i -> if f i then put i (filterStream f) else (filterStream f)

-- Also concatMap.
arrConcat :: (i -> [o]) -> Stream i o
arrConcat f = get $ \i -> puts (f i) (arrConcat f)

-- A mapAccum stream processor. A stream processor with state.
arrAccum :: (k -> i -> (k, o)) -> k -> Stream i o
arrAccum f k = get $ \i -> let (k', o) = (f k i) in put o (arrAccum f k')

-------------------------------------------------------------------------------

data Position = Position {
	line :: Int,
	col :: Int 
} deriving Show

updatePosition :: Position -> Char -> Position
updatePosition (Position ln cl) char = if char == '\n' then (Position (ln+1) 0) else (Position ln (cl+1))

-- Returns n + 1 positions, after and before the char
arrPosition :: Stream Char Position
arrPosition = put (Position 0 0) $ arrAccum (\p c -> let p' = updatePosition p c in (p', p')) (Position 0 0)


