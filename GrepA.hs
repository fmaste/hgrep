
module GrepA () where

-------------------------------------------------------------------------------

import Prelude hiding (id,(.)) -- Using id and . from Category
import Control.Category
import Control.Arrow

-------------------------------------------------------------------------------

type Stream a = [a]

newtype GrepA b c = GrepA {runGrepA :: (Stream b) -> (Stream c)}

instance Category GrepA where
	
	-- id :: cat a a
	id = GrepA id

	-- (.) :: cat b c -> cat a b -> cat a c
	(GrepA f) . (GrepA g) = GrepA (f . g)

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

{-
instance ArrowZero GrepA where
	zeroArrow = GrepA $ \_ -> []

instance ArrowPlus GrepA where
	(GrepA f) <+> (GrepA g) = GrepA $ \bs -> 
-}

instance ArrowLoop GrepA where
	-- loop :: a (b, d) (c, d) -> a b c 
	-- loop :: arrow (input, feedback) (output, feedback) -> arrow input output
	loop (GrepA f) = GrepA $ \as -> let (bs, cs) = unzip (f (zip as (stream cs))) in bs
		where stream ~(x:xs) = x:stream xs

delay :: b -> GrepA b b
delay b = GrepA (b:)

newtype Automaton b c = Automaton (b -> (c, Automaton b c))


