{-# LANGUAGE RankNTypes #-}

import Data.Functor.Identity
import Data.Functor.Const

type Setter' a b s t = forall f. Functor f => (a -> f b) -> s -> f t

fset :: Setter' a b s t -> (a -> b) -> s -> t
fset set f s = runIdentity $ set (Identity . f) s

_fst :: Setter' a b (a, c) (b, c)
_fst a2fb (a, c) = (\x -> (x, c)) <$> a2fb a

data Address = Address { street :: String, suburb :: String } deriving (Show, Eq)
-- data Person = Person { name :: String, address :: Address } deriving (Show, Eq)
data Person = Person { name :: String } deriving (Show, Eq)

type Lens'' s t a b = forall f. Functor f => (a -> f b) -> s -> f t

lname :: Lens'' Person Person String String
lname fname p = (\n -> p { name = n }) <$> fname (name p)

-- laddress :: Lens'' Person Person Address Address
-- laddress faddress p = (\n -> p { address = n}) <$> faddress (address p)

-- lstreet :: Lens'' Address Address String String
-- lstreet fstreet addr = (\a -> addr { street = a }) <$> fstreet (street addr)

mkIdentity a = (undefined, a)
mkConst a = (a, undefined)
dup a = (a, a)

id1 = runIdentity $ lname Identity $ Person "Hackle"
-- id2 = snd $ lname mkIdentity $ Person "Hackle" (Address "Sale" "CBD")

name1 = getConst $ lname Const $ Person "Hackle"
-- name2 = fst $ lname mkConst $ Person "Hackle" (Address "Sale" "CBD")