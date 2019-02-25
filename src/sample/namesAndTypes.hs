type IsFenced = Bool
type Code = String

parseCode :: String -> IsFenced -> Code
parseCode _ _ = ""


newtype CustomerId = CustomerId Int deriving Eq
newtype OrderId = OrderId Int deriving Eq
newtype PromoId = PromoId Int deriving Eq

calculateDiscount :: CustomerId -> OrderId -> PromoId -> Int
calculateDiscount _ _ _ = 1