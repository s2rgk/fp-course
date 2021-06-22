{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.Compose where

import Course.Core
import Course.Functor
import Course.Applicative
import Course.Monad
import Course.Contravariant

-- Exactly one of these exercises will not be possible to achieve. Determine which.

newtype Compose f g a = Compose (f (g a)) deriving (Show, Eq)

-- Implement a Functor instance for Compose
instance (Functor f, Functor g) => Functor (Compose f g) where
  ab <$> (Compose fga) = Compose $ (ab <$>) <$> fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
-- Implement the pure function for an Applicative instance for Compose
  pure = Compose . pure . pure
-- Implement the (<*>) function for an Applicative instance for Compose
-- lift2 ::          (  a     ->  b  ->  c ) -> f       a      -> f   b   -> f   c
-- lift2 ((<*>) :: g (a -> b) -> g a -> g b) :: f (g (a -> b)) -> f (g a) -> f (g b)
  (Compose fgab) <*> (Compose fga) = Compose $ lift2 (<*>) fgab fga
    
instance (Monad f, Monad g) => Monad (Compose f g) where
-- Implement the (=<<) function for a Monad instance for Compose
  afgb =<< (Compose fga) = undefined
  

-- Note that the inner g is Contravariant but the outer f is
-- Functor. We would not be able to write an instance if both were
-- Contravariant; why not?
instance (Functor f, Contravariant g) =>  Contravariant (Compose f g) where
-- Implement the (>$<) function for a Contravariant instance for Compose
  ba >$< (Compose fga) = Compose $ ( ba >$< ) <$> fga