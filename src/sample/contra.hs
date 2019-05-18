{-# Language RankNTypes #-}

functor_to_int :: forall f. Functor f => f Int -> Int
functor_to_int = const 1

applicative_to_int :: forall f. Applicative f => f Int -> Int
applicative_to_int = const 1

any_to_int :: a -> Int
any_to_int = const 1

applicative_to_int__to_int :: (forall f. Applicative f => f Int -> Int) -> Int
applicative_to_int__to_int _ = 1

functor_to_int__to_int :: (forall f. Functor f => f Int -> Int) -> Int
functor_to_int__to_int _ = 1

-- foo :: Int
-- foo = needA fromF

-- bar :: Int
-- bar = needF fromA