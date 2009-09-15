{-# LANGUAGE GADTs #-}

data Step f ns where
  Step :: (a -> (b, ns)) -> Step (a -> b) ns

infixr :.:

type (:.:) f g ns = Step f (g ns) 