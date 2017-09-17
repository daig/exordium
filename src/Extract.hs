module Extract where

class Extract f where
  {-# minimal extractWith | extract #-}
  extractWith :: (a -> b) -> f a -> b
  -- f (extract x) = extract (fmap f x)
  extractWith f x = f (extract x)
  extract :: f a -> a
  extract = extractWith (\x -> x)
  
