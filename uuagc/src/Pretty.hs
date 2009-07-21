-------------------------------------------------------------------------
-- Subset of UU.Pretty, based on very simple pretty printing
-- Extended with line-nr tracking
-------------------------------------------------------------------------

module Pretty
  ( PP_Doc, PP(..)
  , disp

  , (>|<), (>-<)
  , (>#<)
  , ppWithLineNr
  , hlist, vlist, hv
  , fill
  , indent

  , pp_block
  , vlist_sep
  , pp_parens
  , hv_sp

  , empty, text
  )
  where

import Data.List(intersperse)
  
-------------------------------------------------------------------------
-- Doc structure
-------------------------------------------------------------------------

data Doc
  = Emp
  | Str			!String					-- basic string
  | Hor			Doc  !Doc				-- horizontal positioning
  | Ver			Doc  !Doc				-- vertical positioning
  | Ind			!Int Doc				-- indent
  | Line        (Int -> Doc)            -- line nr

type PP_Doc = Doc

-------------------------------------------------------------------------
-- Basic combinators
-------------------------------------------------------------------------

infixr 3 >|<, >#<
infixr 2 >-< 

(>|<) :: (PP a, PP b) => a -> b -> PP_Doc
l >|< r = pp l `Hor` pp r

(>-<) :: (PP a, PP b) => a -> b -> PP_Doc
l >-< r = pp l `Ver` pp r

(>#<) :: (PP a, PP b) => a -> b -> PP_Doc
l >#< r  =  l >|< " " >|< r

indent :: PP a => Int -> a -> PP_Doc
indent i d = Ind i $ pp d

text :: String -> PP_Doc
text s
  = let ls = lines s
        ls' | null ls   = [""]
            | otherwise = ls
    in vlist (map Str ls')

empty :: PP_Doc
empty = Emp

ppWithLineNr :: PP a => (Int -> a) -> PP_Doc
ppWithLineNr f = Line (pp . f)

-------------------------------------------------------------------------
-- Derived combinators
-------------------------------------------------------------------------

hlist, vlist :: PP a => [a] -> PP_Doc
vlist [] = empty
vlist as = foldr  (>-<) empty as
hlist [] = empty
hlist as = foldr  (>|<) empty as

hv :: PP a => [a] -> PP_Doc
hv = vlist

hv_sp :: PP a => [a] -> PP_Doc
hv_sp = foldr (>#<) empty

fill :: PP a => [a] -> PP_Doc
fill = hlist

pp_block:: (PP a, PP b, PP c) => a -> b -> c -> [PP_Doc] -> PP_Doc
pp_block o c s as = pp o >|< hlist (intersperse (pp s) as) >|< pp c

pp_parens :: PP a => a -> PP_Doc
pp_parens p = '(' >|< p >|< ')'

vlist_sep :: (PP a, PP b) => a -> [b] -> PP_Doc
vlist_sep sep lst
  = vlist (intersperse (pp sep) (map pp lst))

-------------------------------------------------------------------------
-- PP class
-------------------------------------------------------------------------

class Show a => PP a where
  pp     :: a   -> PP_Doc
  pp       = text . show

  ppList :: [a] -> PP_Doc
  ppList as = hlist as

instance PP Doc where
  pp     = id

instance PP Char where
  pp c   = text [c]
  ppList = text

instance PP a => PP [a] where
  pp = ppList

instance Show Doc where
  show p = disp p 200 ""

instance PP Int where
  pp = text . show

instance PP Float where
  pp = text . show

-------------------------------------------------------------------------
-- Observation
-------------------------------------------------------------------------

isEmpty :: PP_Doc -> Bool
isEmpty Emp         = True
isEmpty (Ver d1 d2) = isEmpty d1 && isEmpty d2
isEmpty (Hor d1 d2) = isEmpty d1 && isEmpty d2
isEmpty (Ind _  d ) = isEmpty d
isEmpty _           = False

-------------------------------------------------------------------------
-- Rendering
-------------------------------------------------------------------------

disp  ::  PP_Doc -> Int -> ShowS
disp d _ s
  = r
  where (r,_,_) = put 0 1 d s
        put p l d s
          = case d of
              Emp              -> (s,p,l)
              Str s'           -> (s' ++ s,p + length s',l)
              Ind i  d         -> (ind ++ r,p', l')
                               where (r,p',l') = put (p+i) l d s
                                     ind = replicate i ' '
              Hor d1 d2        -> (r1,p2,l2)
                               where (r1,p1,l1) = put p  l  d1 r2
                                     (r2,p2,l2) = put p1 l1 d2 s
              Ver d1 d2 | isEmpty d1
                               -> put p l d2 s
              Ver d1 d2 | isEmpty d2
                               -> put p l d1 s
              Ver d1 d2        -> (r1,p2,l2)
                               where (r1,p1,l1) = put p l d1 $ "\n" ++ ind ++ r2
                                     (r2,p2,l2) = put p (l1+1) d2 s
                                     ind = replicate p ' '
              Line f           -> (r,p',l')
                               where (r,p',l') = put p l (f l) s

