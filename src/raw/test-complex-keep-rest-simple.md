Unit testing can be made cheap and painless, or even joyful if code is written with a conscious segregation of complexity.

The idea is very simple: separate complex logic to simpler units, and make the integration of these units trivial. 

## example: `ShoppingController`

Let's look at an example, a `ShoppingController` for an online shopping application. To calculate total sum to pay for a list of items, the business logic is as follows:

* buyer can get discounts according to type of membership
* it's also possible to get a discount with a promo code
* in case buyer qualifies for both types of discount, the better discount applies
* however, some items are not discountable and their original price should be used

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

As an example this may seem unnecessarily complicated - but we all know that it's nowhere as complicated as real world applications.

I believe we'll all agree that `CalculateTotalPayable` needs to be unit tested thoroughly because it would be quite important to our application and it's fairly complex (and can grow much more so).

## analyzing the complexity of `CalculateTotalPayable`

Let's inspect the complexity of the method and how we can unit test it.

* because the controller depends on `ILoggingService` and `IStockService`, Mocks for these interfaces are to be created to construct the controller. **Although** the method under test has no use for `IStockService`. (see also [inject functions, not interfaces](/inject-functions--not-interfaces))
* there are 3 scenarios for discount based on types of membership 
* also 3 scenarios for discount based on promo code
* yet 2 scenarios based on whether an `Item` can be discounted or not

I'll have to confess I am not looking forward to unit testing this method:

* we need `3 x 3 x 2 = 18` test cases to fully cover this method. (and remember real-life applications can be much more complicated than this!)
* any time a new dependency is introduced to the controller, even if the new dependency has nothing to do with `CalculateTotalPayable`, all the 18 tests will break and needs fixing.
* the call to `ILoggingService.Log` really is trivial - but it also needs to be stubbed or tests will fail.

This is typically the moment teams decide that it's not worthwhile to unit test, as developers explain how complicated, time-consuming it is, and how annoying maintenance will be, "although it would be the **right** thing to do", everybody agrees with regret.

But hold on team! This doesn't have to be the case. We'll see how we can move some code around and make unit testing a breeze. It won't take long - just 2 very simple steps, at most.

## step 1. separate code into pure functions

Looking closely at the method body, it's obvious that it consists of 3 steps:

- calculate discount based on type of membership
- calculate discount based on promo code
- calculate the sum to pay based on whether each item is discountable, and which discount to apply

So step 1 is to move each step into a separate static class. We are not going to use interfaces because:

* static methods are the easiest to unit tests (as they are usually equivalent to pure methods)
* interfaces are also code to write and maintain, and usually lead to [header interfaces](https://martinfowler.com/bliki/HeaderInterface.html)

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

It doesn't get any easier - just only a matter of giving input and expecting output! I describe this type of unit tests as **data-driven**. Such is the beauty of static methods aka pure functions.

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

You'd be surprised - we are done, there is not really a step 2. Except maybe, deleting any unit tests previously written for the original complex `CalculateTotalPayable`?

And yes, you heard right - we are not going to unit test `CalculateTotalPayable`, because it's now **trivial**.

Look at it. There is no branching of logic in this method any longer, it's made up of simple (almost boring) statements and expressions. There is but a single path through this method, or in other words, its [Cyclomatic complexity](https://en.wikipedia.org/wiki/Cyclomatic_complexity) is 1.

But, do we still get the benefits of unit testing, and are we still aligned with best practices? Let's see

* separation of concern: check, we break the complex method into functions with very specific responsibilities.
* completeness of unit testing: check, we still cover all scenarios. Only instead of keeping all logic in a lump, we break them apart so it's much easier to test.

And for extra benefits

* more robust unit tests - being static methods, the code under test is now free of dependencies
* fewer un-robust unit tests - we are not testing the controller now there is no risk of having broken unit tests because of new dependencies introduced to the controller, potentially for unrelated methods.

### the integrator pattern

Taking this style of code and unit testing one step further, we arrive at such a pattern that 

* classes that only integrate other code units, but are devoid of complexity themselves. Examples of such classes: controllers in MVC, presenters in MVP, services in service oriented architecture, or components in some front-end frameworks. They have a very low level of complexity and therefore require no unit testing.

* complexity is separated into code units with specific responsibilities. These classes / functions are where the logic of the application lives and should be thoroughly unit tested. They are preferably static / pure, so unit testing becomes data-driven, or, a matter of coming up with good sets of input and expected output.

We can call this the **Integrator pattern**. The essence of this pattern, as can be seen above, is conscious segregation of complexity. 

## summary

The software industry is notorious for complicating things that should otherwise be simple. Popular practices for unit testing is just another example. 

Unit testing is key to productivity especially when dealing with complexity. However, because of poor practices (and frameworks made to accommodate such practices, which in turn further compound the problems) it has a bad name of being expensive: hard to write, fragile and easily broken, a nightmare to maintain.

The matter is made worse by some popular frameworks that endorse practices that are misleading, confusing and unnecessarily complicated. (Yes I am looking at you, Angular).

The solution is not to resort to fight complexity with more complexity, such as mocking, dependency injection, creating header interfaces. Instead, we can look back at complexity in its face, consciously break it up and segregate it, and quite easily, we restore simplicity and the joy of unit testing. 