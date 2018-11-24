It's an open secret that languages with expressive type systems can make domain modeling much easier. Because I work in csharp a lot, F# is a natural choice to try.

## A strong model

My personal criteria is that if a model can survive the marauding of QuickCheck, then it must be strong.

For example, if we are to model payment methods including cash, bank deposit, and credit card. A typical object oriented language falls short immediately:

```csharp
interface IPayment
{
    public string CreditCardNumber { get; set; }
}

class CreditCardPayment : IPayment
{
    public string CreditCardNumber { get; set; } // not applicable!
}
```

But with a good type system it's almost trivial,

```fsharp
type CreditCardInfo = { CardNumber: int; SecurityCode: int }
type Payment = Cash | Deposit | CreditCard of CreditCardInfo
```

## What's the benefit?

Amongst many other benefits, usually a stronger model requires less code for validation, and therefore less unit testing.
