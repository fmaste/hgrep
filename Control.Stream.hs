
module Control.Stream () where

-------------------------------------------------------------------------------

import Prelude hiding (id, (.)) -- Using id and . from Category
import Control.Category
import Control.Arrow
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
	-- Get something them put it back and continue doing the same.
	id = get $ \b -> put b id

	-- (.) :: cat d c -> cat b d -> cat b c
	-- The input of the first parameter is the output of the second one.
	(Put c s) . stream = put c (s . stream)
	(Get f) . (Put d s) = (f d) . s
	(Get f) . (Get g) = get $ \b -> (get f) . (g b)

-------------------------------------------------------------------------------

instance Arrow Stream where

	-- arr :: (b -> c) -> a b c
	arr f = get $ \b -> put (f b) (arr f)
	-- Builds a stateless process that just applies a given function to its input to
	-- to produce its outputs.

	-- first :: a b c -> a (b, d) (c, d)
	first s = bypass [] s where
		bypass ds (Get f) = get $ \(b, d) -> bypass (ds ++ [d]) (f b)
		bypass (d : ds) (Put c s) = put (c, d) (bypass ds s)
		bypass [] (Put c s) = get $ \(b, d) -> put (c, d) (bypass [] s)
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


