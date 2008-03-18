{-# OPTIONS -fglasgow-exts -cpp #-}
{-# LINE 1 "src/ScannerSheet.x" #-}
module ScannerSheet where


import DocTypes_Generated
import Layout.Scanner

#if __GLASGOW_HASKELL__ >= 603
#include "ghcconfig.h"
#else
#include "config.h"
#endif
#if __GLASGOW_HASKELL__ >= 503
import Data.Array
import Data.Char (ord)
import Data.Array.Base (unsafeAt)
#else
import Array
import Char (ord)
#endif
#if __GLASGOW_HASKELL__ >= 503
import GHC.Exts
#else
import GlaExts
#endif
alex_base :: AlexAddr
alex_base = AlexA# "\xf7\xff\xff\xff\x03\xff\xff\xff\x00\x00\x00\x00\xf9\xff\xff\xff\xe4\xff\xff\xff\x00\x00\x00\x00\x9f\xff\xff\xff\xa5\xff\xff\xff\xa2\xff\xff\xff\x00\x00\x00\x00\x9a\xff\xff\xff\xa0\xff\xff\xff\xda\xff\xff\xff\x00\x00\x00\x00"#

alex_table :: AlexAddr
alex_table = AlexA# "\x00\x00\x03\x00\x02\x00\x03\x00\x04\x00\x05\x00\x06\x00\x07\x00\x09\x00\x0a\x00\x0c\x00\x0c\x00\x0c\x00\x0c\x00\x0c\x00\x0c\x00\x0c\x00\x0c\x00\x0c\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x04\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0d\x00\x0d\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0c\x00\x0c\x00\x0c\x00\x0c\x00\x0c\x00\x0c\x00\x0c\x00\x0c\x00\x0c\x00\x0c\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x0b\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x08\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x00\x02\x00\x00\x00\x00\x00"#

alex_check :: AlexAddr
alex_check = AlexA# "\xff\xff\x0a\x00\xff\x00\x0a\x00\x20\x00\x66\x00\x61\x00\x65\x00\x6e\x00\x69\x00\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\xff\xff\xff\xff\xff\xff\x20\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x28\x00\x29\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x30\x00\x31\x00\x32\x00\x33\x00\x34\x00\x35\x00\x36\x00\x37\x00\x38\x00\x39\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x42\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x4c\x00\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\x00\xff\xff\xff\xff"#

alex_deflt :: AlexAddr
alex_deflt = AlexA# "\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff\xff"#

alex_accept = listArray (0::Int,13) [[],[],[(AlexAcc (alex_action_0))],[(AlexAcc (alex_action_1))],[(AlexAcc (alex_action_2))],[(AlexAcc (alex_action_3))],[],[],[],[(AlexAcc (alex_action_4))],[],[],[(AlexAcc (alex_action_5))],[(AlexAcc (alex_action_6))]]
{-# LINE 23 "src/ScannerSheet.x" #-}

alex_action_0 = mkStructuralToken 
alex_action_1 = collectWhitespace 
alex_action_2 = collectWhitespace 
alex_action_3 = mkToken $ \s -> LeafToken 
alex_action_4 = mkToken $ \s -> BinToken 
alex_action_5 = mkToken $ \s -> IntToken 
alex_action_6 = mkToken $ \s -> SymToken s 
{-# LINE 2 "AlexTemplate-ghc" #-}  -- why do we need to put 2 here instead of 1?

{-
This file is a combination of a wrapper needed for the Proxima scanner, and the template.

Alex does not allow specification of a wrapper file without also providing a template. By putting
both in the template, we avoid needing two files.

The template was copied from a generated Alex scanner with --ghc.
-}

-- -----------------------------------------------------------------------------
-- Proxima wrapper

type ScanChar_ = ScanChar Document Node ClipDoc UserToken
-- ScanChar_ is ScanChar applied to its parameter types. This is necessary, because Alex
-- cannot handle parameters in the AlexInput type.

type AlexInput  = (Char, [ScanChar_])


scanner :: IDPCounter -> [ScanChar_] -> ([Token Document Node ClipDoc UserToken], IDPCounter, WhitespaceMap, WhitespaceFocus)
scanner idPCounter scs = 
  let (mTokens, (tokenPos, idPCounter', whitespaceMap, lastWhitespaceFocus)) = alexScanTokens initScannerState scs
  in  (catMaybes mTokens, idPCounter', whitespaceMap, lastWhitespaceFocus)
 where initScannerState :: ScannerState
       initScannerState = (0, idPCounter, empty,initWhitespaceFocus)

-- The Maybe type in the scanner output is to be able to scan things that do not produce
-- tokens, such as whitespace.  
alexScanTokens :: ScannerState -> [ScanChar_] -> 
                   ([Maybe (Token Document Node ClipDoc UserToken)], ScannerState)
alexScanTokens initState scs = go initState ('\n',scs)
  where go :: ScannerState -> (Char, [ScanChar_]) -> ([Maybe (Token Document Node ClipDoc UserToken)], ScannerState)
        go state@(pos, _, _, _) inp@(_,str) =
	  case alexScan inp 0 of
		AlexEOF -> ([], state)
		AlexError (_,remaining) -> ( [Just $ ErrorTk pos $ stringFromScanChars str ]
		                           , state)
		AlexSkip  inp' len     -> go state inp'
		AlexToken inp' len act -> let (mToken, state') = act state (take len str)
		                              (mTokens, state'') = go state' inp'
		                          in  (mToken : mTokens, state'')  
 
 
 
 
 
 
-- -----------------------------------------------------------------------------
-- ALEX TEMPLATE
--
-- This code is in the PUBLIC DOMAIN; you may copy it freely and use
-- it for any purpose whatsoever.

-- -----------------------------------------------------------------------------
-- INTERNALS and main scanner engine

data AlexAddr = AlexA# Addr#

#if __GLASGOW_HASKELL__ < 503
uncheckedShiftL# = shiftL#
#endif

{-# INLINE alexIndexInt16OffAddr #-}
alexIndexInt16OffAddr (AlexA# arr) off =
#ifdef WORDS_BIGENDIAN
  narrow16Int# i
  where
	i    = word2Int# ((high `uncheckedShiftL#` 8#) `or#` low)
	high = int2Word# (ord# (indexCharOffAddr# arr (off' +# 1#)))
	low  = int2Word# (ord# (indexCharOffAddr# arr off'))
	off' = off *# 2#
#else
  indexInt16OffAddr# arr off
#endif



{-# INLINE alexIndexInt32OffAddr #-}
alexIndexInt32OffAddr (AlexA# arr) off = 
#ifdef WORDS_BIGENDIAN
  narrow32Int# i
  where
   i    = word2Int# ((b3 `uncheckedShiftL#` 24#) `or#`
		     (b2 `uncheckedShiftL#` 16#) `or#`
		     (b1 `uncheckedShiftL#` 8#) `or#` b0)
   b3   = int2Word# (ord# (indexCharOffAddr# arr (off' +# 3#)))
   b2   = int2Word# (ord# (indexCharOffAddr# arr (off' +# 2#)))
   b1   = int2Word# (ord# (indexCharOffAddr# arr (off' +# 1#)))
   b0   = int2Word# (ord# (indexCharOffAddr# arr off'))
   off' = off *# 4#
#else
  indexInt32OffAddr# arr off
#endif




#if __GLASGOW_HASKELL__ < 503
quickIndex arr i = arr ! i
#else
-- GHC >= 503, unsafeAt is available from Data.Array.Base.
quickIndex = unsafeAt
#endif


-- -----------------------------------------------------------------------------
-- Main lexing routines

data AlexReturn a
  = AlexEOF
  | AlexError  !AlexInput
  | AlexSkip   !AlexInput !Int
  | AlexToken  !AlexInput !Int a

-- alexScan :: AlexInput -> StartCode -> AlexReturn a
alexScan input (I# (sc))
  = alexScanUser undefined input (I# (sc))

alexScanUser user input (I# (sc))
  = case alex_scan_tkn user input 0# input sc AlexNone of
	(AlexNone, input') ->
		case alexGetChar input of
			Nothing -> 



				   AlexEOF
			Just _ ->



				   AlexError input'

	(AlexLastSkip input len, _) ->



		AlexSkip input len

	(AlexLastAcc k input len, _) ->



		AlexToken input len k


-- Push the input through the DFA, remembering the most recent accepting
-- state it encountered.

alex_scan_tkn user orig_input len input s last_acc =
  input `seq` -- strict in the input
  let 
	new_acc = check_accs (alex_accept `quickIndex` (I# (s)))
  in
  new_acc `seq`
  case alexGetChar input of
     Nothing -> (new_acc, input)
     Just (c, new_input) -> 



	let
		base   = alexIndexInt32OffAddr alex_base s
		(I# (ord_c)) = ord c
		offset = (base +# ord_c)
		check  = alexIndexInt16OffAddr alex_check offset
		
		new_s = if (offset >=# 0#) && (check ==# ord_c)
			  then alexIndexInt16OffAddr alex_table offset
			  else alexIndexInt16OffAddr alex_deflt s
	in
	case new_s of 
	    -1# -> (new_acc, input)
		-- on an error, we want to keep the input *before* the
		-- character that failed, not after.
    	    _ -> alex_scan_tkn user orig_input (len +# 1#) 
			new_input new_s new_acc

  where
	check_accs [] = last_acc
	check_accs (AlexAcc a : _) = AlexLastAcc a input (I# (len))
	check_accs (AlexAccSkip : _)  = AlexLastSkip  input (I# (len))
	check_accs (AlexAccPred a pred : rest)
	   | pred user orig_input (I# (len)) input
	   = AlexLastAcc a input (I# (len))
	check_accs (AlexAccSkipPred pred : rest)
	   | pred user orig_input (I# (len)) input
	   = AlexLastSkip input (I# (len))
	check_accs (_ : rest) = check_accs rest

data AlexLastAcc a
  = AlexNone
  | AlexLastAcc a !AlexInput !Int
  | AlexLastSkip  !AlexInput !Int

data AlexAcc a user
  = AlexAcc a
  | AlexAccSkip
  | AlexAccPred a (AlexAccPred user)
  | AlexAccSkipPred (AlexAccPred user)

type AlexAccPred user = user -> AlexInput -> Int -> AlexInput -> Bool

-- -----------------------------------------------------------------------------
-- Predicates on a rule

alexAndPred p1 p2 user in1 len in2
  = p1 user in1 len in2 && p2 user in1 len in2

--alexPrevCharIsPred :: Char -> AlexAccPred _ 
alexPrevCharIs c _ input _ _ = c == alexInputPrevChar input

--alexPrevCharIsOneOfPred :: Array Char Bool -> AlexAccPred _ 
alexPrevCharIsOneOf arr _ input _ _ = arr ! alexInputPrevChar input

--alexRightContext :: Int -> AlexAccPred _
alexRightContext (I# (sc)) user _ _ input = 
     case alex_scan_tkn user input 0# input sc AlexNone of
	  (AlexNone, _) -> False
	  _ -> True
	-- TODO: there's no need to find the longest
	-- match when checking the right context, just
	-- the first match will do.

-- used by wrappers
iUnbox (I# (i)) = i
