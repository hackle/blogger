{-# LANGUAGE RankNTypes #-}

import Data.Functor.Identity
import Data.Functor.Const

type Setter' a b s t = forall f. Functor f => (a -> f b) -> s -> f t

fset :: Setter' a b s t -> (a -> b) -> s -> t
fset set f s = runIdentity $ set (Identity . f) s

_fst :: Setter' a b (a, c) (b, c)
_fst a2fb (a, c) = (\x -> (x, c)) <$> a2fb a

data Address = Address { street :: String, suburb :: String } deriving (Show, Eq)
data Person = Person { name :: String, address :: Address } deriving (Show, Eq)

type Lens'' s t a b = forall f. Functor f => (a -> f b) -> s -> f t

lname :: Lens'' Person Person String String
lname fname p = (\n -> p { name = n }) <$> fname (name p)

laddress :: Lens'' Person Person Address Address
laddress faddress p = (\n -> p { address = n}) <$> faddress (address p)

lstreet :: Lens'' Address Address String String
lstreet fstreet addr = (\a -> addr { street = a }) <$> fstreet (street addr)

id1 = runIdentity $ lname Identity $ Person "Hackle" (Address "Sale" "CBD")
id2 = snd $ lname (\a -> (undefined, a)) $ Person "Hackle" (Address "Sale" "CBD")

name1 = getConst $ lname Const $ Person "Hackle" (Address "Sale" "CBD")
name2 = fst $ lname (\a -> (a, undefined)) $ Person "Hackle" (Address "Sale" "CBD")