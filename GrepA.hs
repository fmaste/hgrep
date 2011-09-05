
module GrepA () where

-------------------------------------------------------------------------------

import Prelude hiding (id, (.)) -- Using id and . from Category
import Control.Category
import Control.Arrow
import Control.Arrow.Stream
import Data.Either
import Data.Word
import Data.Bits

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

{-
data UTF8State = UTF8State BytesSequence BytesRead Word | Error
	deriving Show
-- Reading a sequence of one, two, three or four bytes.
newtype BytesSequence = BytesSequence Number
	deriving Show
-- Reading byte number one, two, three or four of the designated sequence.
newtype BytesRead = BytesRead Number
	deriving Show
-- Enumeration
data Number = One | Two | Three | Four
	deriving Show

firstByte :: Word8 -> UTF8State
firstByte w 
	| w <= 0x7F = UTF8State (BytesSequence One)   (BytesRead One) $ w .&. 0xEF -- 0xxxxxxx
	| w <= 0xDF = UTF8State (BytesSequence Two)   (BytesRead One) $ w .&. 0x1F -- 110xxxxx
	| w <= 0xEF = UTF8State (BytesSequence Three) (BytesRead One) $ w .&. 0x0F -- 1110xxxx
	| w <= 0xF7 = UTF8State (BytesSequence Four)  (BytesRead One) $ w .&. 0x0F -- 11110xxx
	| otherwise = Error

isTailByte :: Word8 -> Bool
isTailByte w = w >= 0x80 -- 10xxxxxx

appendTailByte :: Word -> Word8 -> Word
appendTailByte w w' = w 

updateState :: UTF8State -> Word8 -> UTF8State
updateState (UTF8State (BytesSequence One  ) (BytesRead One  ) w) w' = firstByte w'
updateState (UTF8State (BytesSequence Two  ) (BytesRead One  ) w) w'
	| isTailByte w' = UTF8State (BytesSequence Two  ) (BytesRead Two  ) (appendTailByte w w')
	| otherwise = Error
updateState (UTF8State (BytesSequence Two  ) (BytesRead Two  ) w) w' = firstByte w'
updateState (UTF8State (BytesSequence Three) (BytesRead One  ) w) w'
	| isTailByte w' = UTF8State (BytesSequence Three) (BytesRead Two  ) (appendTailByte w w')
	| otherwise = Error
updateState (UTF8State (BytesSequence Three) (BytesRead Two  ) w) w'
	| isTailByte w' = UTF8State (BytesSequence Three) (BytesRead Three) (appendTailByte w w') 
	| otherwise = Error
updateState (UTF8State (BytesSequence Three) (BytesRead Three) w) w' = firstByte w'
updateState (UTF8State (BytesSequence Four ) (BytesRead One  ) w) w'
	| isTailByte w' = UTF8State (BytesSequence Four ) (BytesRead Two  ) (appendTailByte w w')
	| otherwise = Error
updateState (UTF8State (BytesSequence Four ) (BytesRead Two  ) w) w'
	| isTailByte w' = UTF8State (BytesSequence Four ) (BytesRead Three) (appendTailByte w w')
	| otherwise = Error
updateState (UTF8State (BytesSequence Four ) (BytesRead Three) w) w'
	| isTailByte w' = UTF8State (BytesSequence Four ) (BytesRead Four ) (appendTailByte w w')
	| otherwise = Error
updateState (UTF8State (BytesSequence Four) (BytesRead Four) w1) w = firstByte w

--utf8 :: Stream Word8 Char
--utf8 
-}

