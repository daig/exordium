module Type.Re where

newtype Re p s t a b = Re {runRe :: p b a -> p t s}
