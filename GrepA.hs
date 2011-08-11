
module GrepA () where

-------------------------------------------------------------------------------

import Prelude hiding (id,(.)) -- Using id and . from Category
import Control.Category
import Control.Arrow

-------------------------------------------------------------------------------

type Stream a = [a]

newtype GrepA b c = GrepA {runGrepA :: (Stream b) -> (Stream c)}

instance Category GrepA where
	id = GrepA id
	(GrepA f) . (GrepA g) = GrepA (f . g)

instance Arrow GrepA where
	arr f = GrepA (map f)
	first (GrepA f) = GrepA $ \bds -> let (bs, ds) = unzip bds in zip (f bs) ds
	second (GrepA f) = GrepA $ \dbs -> let (ds, bs) = unzip dbs in zip ds (f bs)
	(GrepA f) *** (GrepA g) = GrepA $ \bbs -> let (bs, bs') = unzip bbs in zip (f bs) (g bs')
	(GrepA f) &&& (GrepA g) = GrepA $ \bs -> zip (f bs) (g bs)

instance ArrowZero GrepA where
	zeroArrow = GrepA $ \_ -> []

{-
instance ArrowPlus GrepA where
	(GrepA f) <+> (GrepA g) = GrepA $ \bs -> 
-}

newtype Automaton b c = Automaton (b -> (c, Automaton b c))


