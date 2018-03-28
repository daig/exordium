module X.Optic.Equality where
import Data.Proxy as X (Proxy)

type ((s :: k) ==~. (a :: k)) (b :: k') (t :: k') = Identical a (Proxy b) a (Proxy b) -> Identical a (Proxy b) s (Proxy t)
type (s :: k) ==~~. (a :: k) = Identical a (Proxy a) a (Proxy a) -> Identical a (Proxy a) s (Proxy s)

data Identical a b s t where Identical :: Identical a b a b
