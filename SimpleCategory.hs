
module SimpleCategory where

-- | Objects are abstract; we represent them as type parameters.
--   Morphisms are just functions in Haskell.
--   Category laws are satisfied by Haskell's (.) and id.

-- Category typeclass (close to the standard one)
class Category cat where
  idC  :: cat a a
  (⊚)  :: cat b c -> cat a b -> cat a c  -- composition (Unicode)
  -- ASCII fallback: (.) :: cat b c -> cat a b -> cat a c

-- Instance for Haskell functions
instance Category (->) where
  idC = id
  (⊚) = (.)

-- Functor maps types and functions
class FunctorF f where
  fmapF :: (a -> b) -> f a -> f b

-- Simple Maybe functor
instance FunctorF Maybe where
  fmapF _ Nothing  = Nothing
  fmapF g (Just x) = Just (g x)

-- Natural transformation between functors f ⇒ g
type Nat f g = forall a. f a -> g a

-- Monad example using ASCII-compliant bind and return
class MonadM m where
  returnM :: a -> m a
  bindM   :: m a -> (a -> m b) -> m b

instance MonadM Maybe where
  returnM = Just
  bindM Nothing  _ = Nothing
  bindM (Just x) f = f x
