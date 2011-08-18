
module GrepA () where

-------------------------------------------------------------------------------

import Prelude hiding (id, (.)) -- Using id and . from Category
import Control.Category
import Control.Arrow
import Data.Either

-------------------------------------------------------------------------------

newtype GrepA b c = GrepA {runGrepA :: [b] -> [c]}

-- Another way to represent stream processors is using the datatype:
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
runStream (Put c s) bs 	   = c : runStream s bs
runStream (Get k)   (b:bs) = runStream (k b) bs
runStream (Get k)   [] 	   = []
-- We concern ourselfs for the time being with processes that have one input
-- channel and one output channel. 
-- For simplicity we shall only consider non-terminating (recursively defined) 
-- stream processors; otherwise we would add another operator to construct a
-- stream processor which halts.

instance Category GrepA where
	
	-- id :: cat a a
	id = GrepA id

	-- (.) :: cat b c -> cat a b -> cat a c
	(GrepA f) . (GrepA g) = GrepA (f . g)

instance Category Stream where

        -- id :: cat a a
	-- Get something them put it back and continue doing the same.
        id = get $ \b -> put b id

        -- (.) :: cat d c -> cat b d -> cat b c
	(Put c s) . stream = put c (s . stream)
	(Get f) . (Put d s) = (f d) . s
	(Get f) . (Get g) = get $ \b -> (get f) . (g b)

instance Arrow GrepA where

	-- arr :: (b -> c) -> a b c
	arr f = GrepA (map f)

	-- first :: a b c -> a (b, d) (c, d)
	first (GrepA f) = GrepA $ \bds -> let (bs, ds) = unzip bds in zip (f bs) ds
	-- Can also be defined as: 
	-- first (GrepA f) = GrepA (unzip >>> first f >>> uncurry zip)

	-- second :: a b c -> a (d, b) (d, c)
	second (GrepA f) = GrepA $ \dbs -> let (ds, bs) = unzip dbs in zip ds (f bs)

	-- TODO: Make concurrent, use par
	-- (***) :: a b c -> a b' c' -> a (b, b') (c, c')
	(GrepA f) *** (GrepA g) = GrepA $ \bbs -> let (bs, bs') = unzip bbs in zip (f bs) (g bs')
	-- Can also be defined as:
	-- (GrepA f) *** (GrepA g) = GrepA $ first f >>> second g

	-- TODO: Make concurrent, use par
	-- (&&&) :: a b c -> a b c' -> a b (c, c')
	(GrepA f) &&& (GrepA g) = GrepA $ \bs -> zip (f bs) (g bs)
	-- Can also be defined as: 
	-- (GrepA f) &&& (GrepA g) = GrepA $ f &&& g >>> uncurry zip

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
	-- 	where 	swap :: (x,y) -> (y,x)
	-- 		swap ~(x,y) = (y,x)

	-- (***) :: a b c -> a b' c' -> a (b, b') (c, c')
	-- Using default: 
	-- f *** g = first f >>> second g

	-- (&&&) :: a b c -> a b c' -> a b (c, c')
	-- Using default: 
	-- f &&& g = arr (\b -> (b,b)) >>> f *** g

constStream :: c -> Stream b c
constStream c = get $ \b -> put c (constStream c)

mapStream :: (b -> c) -> Stream b c
mapStream f = get $ \b -> put (f b) $ mapStream f

filterStream :: (b -> Bool) -> Stream b b
filterStream f = get $ \b -> if f b then put b (filterStream f) else (filterStream f)

nullStream :: Stream b c
nullStream = get $ \b -> nullStream

{-
instance ArrowZero GrepA where
	zeroArrow = GrepA $ \_ -> []

instance ArrowPlus GrepA where
	(GrepA f) <+> (GrepA g) = GrepA $ \bs -> 
-}

{-
instance ArrowChoice GrepA where

	-- left :: a b c -> a (Either b d) (Either c d)
	left (GrepA f) = GrepA $ map (either (Right . f) (Left . id))

	-- right :: a b c -> a (Either d b) (Either d c)
	right (GrepA f) = GrepA $ either (Left . id) (Right . f)

	-- (+++) :: a b c -> a b' c' -> a (Either b b') (Either c c')
	(GrepA f) +++ (GrepA g) = GrepA $ either (Right . f) (Left . g)

	-- (|||) :: a b d -> a c d -> a (Either b c) d
	(GrepA f) ||| (GrepA g) = GrepA $ either f g
-}

instance ArrowLoop GrepA where
	-- loop :: a (b, d) (c, d) -> a b c 
	-- loop :: arrow (input, feedback) (output, feedback) -> arrow input output
	loop (GrepA f) = GrepA $ \as -> let (bs, cs) = unzip (f (zip as (stream cs))) in bs
		where stream ~(x:xs) = x:stream xs

delay :: b -> GrepA b b
delay b = GrepA (b:)

data Position = Position Int Int
	deriving Show

updatePosition :: GrepA (Char, Position) Position
updatePosition = arr $ \(w, (Position l c)) -> if w == '\n' then Position (l + 1) 0 else Position l (c + 1)

parsePosition :: GrepA Char Position
parsePosition = loop (updatePosition >>> (arr id &&& delay (Position 0 0)))

parseChar :: GrepA Char Char
parseChar = arr id

parse :: GrepA Char (Char, Position)
parse = (parseChar &&& parsePosition)

