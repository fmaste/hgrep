
module Control.Arrow.Stream (
	Stream,
	put,
	get) where

-------------------------------------------------------------------------------

import Prelude hiding (id, (.)) -- Using id and . from Category
import Control.Category
import Control.Arrow
import Data.Sequence
import Data.Either

-------------------------------------------------------------------------------

-- One way to represent stream processors is using the datatype:
data Stream b c = Put c (Stream b c) | Get (b -> Stream b c)
-- where Put b f represents a stream processor that is ready to output c and
-- continue with f, and Get k represents a stream processor waiting for an
-- input b, which will continue by passing it to k.
-- Stream processors are them constructed using the following continuation 
-- style operations for these actions:
put :: c -> Stream b c -> Stream b c
put = Put
-- which constructs a stream processor which outputs the b and them behaves
-- like the second argument, and
get :: (b -> Stream b c) -> Stream b c
get = Get
-- which constructs a stream processor which waits for an input, passes it
-- to its function argument, and them behaves like the result.
-- Stream processors can be interpreted as stream functions by the function:
runStream :: Stream b c -> [b] -> [c]
runStream (Put c s) bs     = c : runStream s bs
runStream (Get k)   (b:bs) = runStream (k b) bs
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
	id = get $ \b -> put b id

	-- (.) :: cat d c -> cat b d -> cat b c
	-- Serial composition of stream processes.
	-- The input of the first parameter is the output of the second one.
	(Put c s) . stream = put c (s . stream)
	(Get f) . (Put d s) = (f d) . s
	(Get f) . (Get g) = get $ \b -> (get f) . (g b)

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
	arr f = get $ \b -> put (f b) (arr f)

	-- first :: a b c -> a (b, d) (c, d)
	-- Builds a process that feeds the first components of its inputs
	-- through its argument process, while the second components bypass 
	-- the process and are recombined with its outputs.
	first s = bypass empty s where
		bypass q (Get f) = get $ \(b, d) -> bypass (q |> d) (f b)
		bypass q (Put c s) = 
			case viewl q of
				EmptyL -> get $ \(b, d) -> put (c, d) (bypass empty s)
				d :< q' -> put (c, d) (bypass q' s)
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
        (Put c s) <+> s' = put c (s <+> s')
	s <+> (Put c s') = put c (s <+> s') 
	(Get f) <+> (Get g) = get $ \b -> (f b) <+> (g b)

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
	left (Put c s) = put (Left c) (left s)
	left (Get f) = get $ either 
				(\b -> left (f b)) 
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

delay :: b -> Stream b b
delay b = put b id

-------------------------------------------------------------------------------

-- Stream utility functions:

-- Continuation passing style put for many elements.
puts :: [c] -> Stream b c -> Stream b c
puts [] s = s
puts (c:cs) s = put c (putList cs s)

constStream :: c -> Stream b c
constStream c = get $ \b -> put c (constStream c)

filterStream :: (b -> Bool) -> Stream b b
filterStream f = get $ \b -> if f b then put b (filterStream f) else (filterStream f)

-- Also concatMap.
arrConcat :: (b -> [c]) -> Stream b c
arrConcat f = get $ \b -> puts (f b) (arrConcat f)

-- A mapAccum stream processor. A stream processor with state.
arrAccum :: (k -> b -> (k, c)) -> k -> Stream b c
arrAccum f k = get $ \b -> let (k', c) = (f k b) in put c (arrAccum f k')

-------------------------------------------------------------------------------

data Position = Position Int Int
	deriving Show

updatePosition (Position ln cl) char = if char == '\n' then (Position (ln+1) 0) else (Position ln (cl+1))

arrPosition = delay (Position 0 0) . arrAccum (\p c -> let p' = updatePosition p c in (p', p')) (Position 0 0)

