
module GrepA () where

-------------------------------------------------------------------------------

import Prelude hiding (id,(.)) -- Using id and . from Category
import Control.Category
import Control.Arrow
import Data.Either

-------------------------------------------------------------------------------

newtype GrepA b c = GrepA {runGrepA :: [b] -> [c]}

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

