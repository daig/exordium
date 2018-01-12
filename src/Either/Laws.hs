module Either.Laws where
import Bool
import Either

-- biextract f _ (inL a) = f a
-- biextract _ g (inR b) = g b
-- biextract m n (bimap f g ) = biextract (m . f) (n . g)

bimapCodiag :: (Either f, Eq a) => (x -> a) -> f x x -> Bool
bimapCodiag f faa = either f f faa == f (codiag faa)
