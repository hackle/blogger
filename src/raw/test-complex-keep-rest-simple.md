Unit testing can be made cheap and painless, or even joyful if code is written with a conscious segregation of complexity.

The idea is very simple: separate complex logic to simpler units, and make the integration of these units trivial. 

## example: `ShoppingController`

Let's look at an example, a `ShoppingController` for an online shopping application. To calculate total sum to pay for a list of items, the business logic is as follows:

* buyer can get a discount according to type of membership
* it's also possible to get a discount with a promo code
* in case buyer qualifies for both types of discount, the better discount applies
* however, some items are not discountable and their original price should apply

```csharp
public class ShoppingController : BaseController
{
    public ShoppingController(ILoggingService loggingService, IStockService stockService)
    {
        // ...
    }

    public decimal CalculateTotalPayable(IEnumerable<Item> items, Membership member, string promoCode) 
    {
        var memberDiscountPercentage = 0;
        switch (member.Type)
        {
            case MemberType.Diamond: 
                memberDiscountPercentage = 10;
                break;
            case MemberType.Gold:
                memberDiscountPercentage = 5;
                break;
            default: 
                break;
        }

        var promoDiscountPercentage = 0;
        if (promoCode == "akaramba")
        {
            promoDiscountPercentage = 8;
        }
        else if (promoCode == "excellent")
        {
            promoDiscountPercentage = 6;
        }

        var discountToApply = Math.Max(memberDiscountPercentage, promoDiscountPercentage);
        var totalPayable = items.Sum(item => {
            if (item.IsDiscountable)
                return item.Price * (1.0 - discountToApply / 100);
            else 
                return item.Price;
        });

        this.loggingService.Log(LogLevel.Info, $"logging that member XYZ gets prices for ABC with dicount so and so");

        return totalPayable;
    }

    // ... other methods
}
```

As an example this may seem unnecessarily complicated - but we all know it's nowhere as complicated as real world applications.

There should be no doubt `CalculateTotalPayable` needs to be unit tested thoroughly, because it's quite important to a shopping application, and it's fairly complex (and can grow much more so).

## analyzing the complexity of `CalculateTotalPayable`

Let's get a feel of the complexity of the method in light of unit testing.

* because the controller depends on `ILoggingService` and `IStockService`, `mocks` for these interfaces are to be created to construct the controller. **Although** `CalculateTotalPayable` has no use for `IStockService`. (injecting interfaces is questionable too - see also [inject functions, not interfaces](/inject-functions--not-interfaces))
* there are 3 scenarios for discount based on types of membership 
* also 3 scenarios for discount based on promo code
* yet 2 more scenarios based on whether an `Item` can be discounted

I'll have to confess I am not looking forward to unit testing this method:

* we need `3 x 3 x 2 = 18` test cases to fully cover this method. (and remember real-life applications can be much more complex!)
* any time a new dependency is added to the controller, even one that has nothing to do with `CalculateTotalPayable`, all the 18 tests will break and need fixing.
* the call to `ILoggingService.Log` really is trivial - but it also needs to be stubbed or tests will fail.

This is typically the moment teams decide that it's not worthwhile to write any unit test, as developers explain how complicated, time-consuming it is, and how annoying maintenance will be, "although it would be the **right** thing to do", everybody agrees with regret.

This is unfortunate and seems paradoxical: complex code is not tested because it's complex, but what's the point of testing if we only test simple code?

But hold on team! This doesn't have to be the case. We'll see how we can move some code around and make unit testing a breeze. It won't take long - just 2 very simple steps, at most.

## step 1. separate code into pure functions

Looking closely at `CalculateTotalPayable`, it's clear that it's made up of 3 steps:

- calculate discount based on type of membership
- calculate discount based on promo code
- calculate the sum to pay based on whether each item is discountable, and which discount to apply

So step 1 is to move each step into a separate static class - not interfaces, because:

* static methods are almost pure functions, which are the easiest to unit test
* interfaces are also code to write and maintain, and usually lead to [header interfaces](https://martinfowler.com/bliki/HeaderInterface.html). Less code is usually better than more.

So we create three separate static classes as follows, to start with discount based on member types:

```csharp
public static class MemberDiscount 
{
    public static int GetInPercentage(Membership member)
    {
        switch (member.Type)
        {
            case MemberType.Diamond: return 10;
            case MemberType.Gold: return 5;
            default: return 0;
        }
    }
}
```

Let's unit test it,

```csharp
[Theory]
[InlineData(MemberType.Diamond, 10)]
[InlineData(MemberType.Gold, 5)]
[InlineData(MemberType.Respected, 0)]
public void Gets_discounts_per_type_of_membership(
    MemberType memberType,
    int expectedDiscount)
{
    var member = new Membership { Type = memberType };
    var actual = MemberDiscount.GetInPercentage(member);
    Assert.Equal(actual, expectedDiscount);
}
```

It doesn't get any easier - only a matter of giving input and expecting output! I consider this type of unit tests **data-driven**. Such is the beauty of static methods aka pure functions.

Same goes for the other two classes.

```csharp
public static class PromoCodeDiscount
{
    public static int GetInPercentage(string promoCode)
    {        
        if (promoCode == "akaramba")
        {
            return 8;
        }
        else if (promoCode == "excellent")
        {
            return 6;
        }

        return 0;
    }
}

public static class ItemSalePrice
{
    public static decimal Calculate(
        Item item, 
        decimal memberDiscountPercentage, 
        decimal promoDiscountPercentage)
    {
        var discountToApply = Math.Max(memberDiscountPercentage, promoDiscountPercentage);
        
        if (item.IsDiscountable)
            return item.Price * (1.0 - discountToApply / 100);
        else 
            return item.Price;
    }
}
```

I'll leave the joy of testing these two methods to you. Here is the challenge: if the method body of each unit test is longer than 3 lines, you are not doing it right.

Finally `CalculateTotalPayable` has a new look: 

```csharp
public decimal CalculateTotalPayable(IEnumerable<Item> items, Membership member, string promoCode) 
{
    var memberDiscountPercentage = MemberDiscount.GetInPercentage(member);
    var promoDiscountPercentage = PromoCodeDiscount.GetInPercentage(promoCode);

    var totalPayable = items.Sum(item => ItemSalePrice.Calculate(item, memberDiscountPercentage, promoDiscountPercentage));

    this.loggingService.Log(LogLevel.Info, $"logging ...");

    return totalPayable;
}
```

## step 2? there is no step 2

You'd be surprised - we are done, there is not really a step 2. Except maybe, deleting any unit tests previously written for the original `CalculateTotalPayable`?

And yes, you guessed right - we are not going to unit test `CalculateTotalPayable`, because it's now **trivial**.

Look at it. There is no branching of logic in this method any longer, it's made up of simple (almost boring) statements and expressions. There is but a single path through this method, or in other words, its [Cyclomatic complexity](https://en.wikipedia.org/wiki/Cyclomatic_complexity) is 1.

Unit testing such methods are still very possible, but usually uninteresting and almost pointless. 

### the integrator pattern

Taking this style of coding and unit testing one step further, we arrive at a pattern that features: 

* a type of classes that only integrate other code units, but are devoid of complexity themselves. Examples of such classes: controllers in MVC, presenters in MVP, services in service oriented architecture, or components in some front-end frameworks. They have a very low level of complexity and therefore require no unit testing.

* complexity is separated into code units with specific responsibilities. These classes / functions are where the logic of the application lives and should be thoroughly unit tested. They are preferably static / pure, so unit testing becomes data-driven, or, a matter of coming up with representative (if not complete) sets of input and expected output.

Let's call this the **Integrator pattern**.

## summary

The software industry is notorious for complicating things that should otherwise be simple. Some of the more popular practices for unit testing is just another example. 

Unit testing is key to productivity especially when dealing with complexity. However, because of poor practices it has a bad name of being expensive: hard to write, fragile and easily broken, a nightmare to maintain.

The matter is made worse by some popular frameworks that endorse practices that try to manage complexity with more complexity, such as built-in support for mocking, dependency injection etc. Now developers need to learn yet another new way to unit test, which does not help and compounds the problems more. (Yes I am looking at you, Angular).

The **Integrator** pattern, as can be seen above, handles complexity by looking right in its face, consciously breaking it up, putting it in separate places, and quite easily, we restore simplicity as well as the joy of unit testing.

It's language- or framework- agnostic, so we can all start using it now. Simplicity, the essence of programming, prevails. Happy hacking!