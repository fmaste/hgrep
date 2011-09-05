
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

-------------------------------------------------------------------------------

-- Generic automaton state
data State s = Init | State s | Error
	deriving Show

-- Specific UTF8 automaton state
data UTFState = UTFState BytesSequence BytesRead Word
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

updateState :: State UTFState -> Word8 -> (State UTFState, [Word])
updateState (State (UTFState (BytesSequence Two  ) (BytesRead One  ) w)) w8
	| isTailByte w8 = 
				let w' = appendTailByte w w8
				in (State $ UTFState (BytesSequence Two  ) (BytesRead Two  ) w', [w'])
	| otherwise = (Error, [])
updateState (State (UTFState (BytesSequence Three) (BytesRead One  ) w)) w8
	| isTailByte w8 = 
				let w' = appendTailByte w w8
				in (State $ UTFState (BytesSequence Three) (BytesRead Two  ) w', [  ])
	| otherwise = (Error, [])
updateState (State (UTFState (BytesSequence Three) (BytesRead Two  ) w)) w8
	| isTailByte w8 = 
				let w' = appendTailByte w w8
				in (State $ UTFState (BytesSequence Three) (BytesRead Three) w', [w'])
	| otherwise = (Error, [])
updateState (State (UTFState (BytesSequence Four ) (BytesRead One  ) w)) w8
	| isTailByte w8 = 
				let w' = appendTailByte w w8
				in (State $ UTFState (BytesSequence Four ) (BytesRead Two  ) w', [  ])
	| otherwise = (Error, [])
updateState (State (UTFState (BytesSequence Four ) (BytesRead Two  ) w)) w8
	| isTailByte w8 = 
				let w' = appendTailByte w w8
				in (State $ UTFState (BytesSequence Four ) (BytesRead Three) w', [  ])
	| otherwise = (Error, [])
updateState (State (UTFState (BytesSequence Four ) (BytesRead Three) w)) w8
	| isTailByte w8 = 
				let w' = appendTailByte w w8
				in (State $ UTFState (BytesSequence Four ) (BytesRead Four ) w', [w'])
	| otherwise = (Error, [])
updateState _ w8
	| otherwise = firstByte w8

firstByte :: Word8 -> (State UTFState, [Word])
firstByte w8
	| w8 <= 0x7F = -- 0xxxxxxx
			let w = fromIntegral w8
			in (State $ UTFState (BytesSequence One)   (BytesRead One) w, [w])
	| w8 <= 0xBF = -- 10xxxxxx
			(Error, [])
	| w8 <= 0xDF = -- 110xxxxx
			let w = (fromIntegral w8) .&. 0x1F
			in (State $ UTFState (BytesSequence Two)   (BytesRead One) w, [ ])
	| w8 <= 0xEF = -- 1110xxxx
			let w = (fromIntegral w8) .&. 0x0F
			in (State $ UTFState (BytesSequence Three) (BytesRead One) w, [ ])
	| w8 <= 0xF7 = -- 11110xxx
			let w = (fromIntegral w8) .&. 0x0F
			in (State $ UTFState (BytesSequence Four)  (BytesRead One) w, [ ])
	| otherwise = 
			(Error, [])

isTailByte :: Word8 -> Bool
isTailByte w = w >= 0x80 && w <= 0xBF -- 10xxxxxx

appendTailByte :: Word -> Word8 -> Word
appendTailByte w w' = w

utf8 :: Stream Word8 Word
utf8 = arrAccumConcat (\s i -> updateState s i) Init

