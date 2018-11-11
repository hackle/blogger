data Cont r a = Cont { unCont:: ((a -> r) -> r) }

runCont :: Cont r a -> (a -> r) -> r
runCont k f = unCont k $ f

instance Functor (Cont r) where
  a2b `fmap` m = Cont $ \b2r -> runCont m $ \a -> b2r (a2b a)

instance Applicative (Cont r) where
  pure a = Cont $ \a2r -> a2r a
  ma2b <*> m = Cont $ \b2r -> runCont m $ \a -> runCont ma2b $ \a2b -> b2r (a2b a)

instance Monad (Cont r) where
  return = pure
  ma >>= a2mb = Cont $ \b2r -> runCont ma $ \a -> runCont (a2mb a) $ \b -> b2r b

myCallCC :: ((a -> Cont r b) -> Cont r a) -> Cont r a
myCallCC a2mb2ma =
  Cont $ \a2r -> let  a2mb = \a1 -> Cont $ \_ -> a2r a1
                      ma = a2mb2ma a2mb
                      in runCont ma undefined
