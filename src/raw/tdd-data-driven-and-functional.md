If unit tests are data-driven, and code under test is a pure function, then Test Driven Development becomes more practical.

## T.D.D. the usual way

Popular practices in Object Oriented Programming using dependency injection and mocking can make it hard to develop in a Test-Driven fashion.

Let's say we need to build a `Checkout` feature for a `ShoppingService`. If we are to test drive the usual way, we'll start a test method, usually with a base case,`If_buying_nothing_Then_charged_for_nothing`, which can be,

```csharp
[Fact]
public void If_buying_nothing_Then_charged_for_nothing()
{
    new ShoppingService().Checkout(new int[] {});
    // ??
}
```

We are stuck - we need a way to verify that a `Charge` method is not called. We go on to create an interface,

```csharp
interface IPaymentService 
{
    void Charge(decimal amount);
}
```

And carry on with our test,

```csharp
[Fact]
public void If_buying_nothing_Then_charged_for_nothing()
{
    var paymentServiceMock = Mock.Of<IPaymentService>();
    new ShoppingService(chargeMock.Object).Checkout(new int[] {});
    
    paymentServiceMock.Verify(c => c.Charge(0), Times.Never);
}
```

Next case if member is buying something,

```csharp
[Fact]
public void If_buying_an_item_Then_charged_for_it()
{
    var paymentServiceMock = Mock.Of<IPaymentService>();
    new ShoppingService(chargeMock.Object).Checkout(new int[] { 1323 });

    // ?? get items
    
    paymentServiceMock.Verify(c => c.Charge(0), Times.Never);
}
```

We realise we need to get `Item`s by `int` IDs - this calls for something like `IItemRepository`. Oh well, we go away and create it,

```csharp
interface IItemRepository
{
    Item[] GetItemsByIDs(int[] itemIDs);
}
```

And we can carry on

```csharp
[Fact]
public void If_buying_an_item_Then_charged_for_it()
{
    var paymentServiceMock = new Mock<IPaymentService>();
    var itemRepositoryMock = new Mock<IItemRepository>();
    var itemIDs = new int[] { 1323 };
    var items = new Item[] { new Item { Price = 100m } };
    itemRepositoryMock.Setup(it => it.GetItemsByIDs(itemIDs)).Returns(items);

    new ShoppingService(chargeMock.Object, itemRepositoryMock.Object).Checkout(new int[] { 1323 });
    
    paymentServiceMock.Verify(c => c.Charge(100m), Times.Once);
}
```

Only that next we need another `ILoggingService` and a `IMemberRepository` for logging and member profile - it's not hard to imagine how bloated the tests can get and how tedious this back-and-forth can be!

The pain points with this approach are

* a lot of going back and forth from test to code under test and to creating interfaces. To be fair, this counts as "test driving code", but it feels heavy-weight.
* each new test breaks the previous ones
* each new test becomes more tedious and bloated than the previous

Sure test can drive code, but need it necessarily be so difficult? Of course not, if we make but one key observation first.

## observation: decision-making needs just data 

Most complexity in day-to-day applications come from decision-making, and *decision-making needs data*, not *how data is provided*.

Consider the below example,

```csharp
public void Checkout(IEnumerable<int> itemIDs, int memberID, DateTime when) 
{
    var member = this.memberRepo.FindByID(memberID);
    var items = this.itemRepo.FindByIDs(itemIDs);

    // decision-making here
    var discountToApply = 0;
    var isBirthday = member.Birthday.Month == when.Month && member.Birthday.Date == when.Date;
    if (isBirthday)
    {
        discountToApply = 50;
    }

    var totalPayable = items.Sum(item => item.Price * (100 - discountToApply) / 100);

    this.paymentService.Charge(memberID, totalPayable);
}
```

The decision-making is in `discountToApply` then `totalPayable`, which depend on `member` and `items`. It's not hard to see that the algorithm to calculate `totalPayable` does not really care where `member` and `items` come from. In the above code, they are from `Repo`s, but if they are from a web service, the algorithm wouldn't need to change much.

What can we do with this observation for unit testing?

## T.D.D. is not for everything

This will sound controversial, but &test-driven unit testing is not for every piece of code*, consider a method that calls a stored procedure in a database, or an external web service. They are I.O. operations, and are possibly straightforward for the client side (usually cross-cutting concerns like security, marshalling are handled by libraries).

The popular saying that "You are not allowed to write any production code unless it is to make a failing unit test pass." is simply not applicable to everything.

If we are to take this further, then anything not essential to *complex decision-making* will not make good candidate for T.D.D. For example, simple getters and setters like `string Foo { get; set; }`, or trivial algorithms like `Foo > Bar`.

However, T.D.D. does make life easier for us when handling complexity. Usually, we first need to separate decision-making to its own place, either from operations like I.O., or other decision-makings.

This is how it works in real life. We don't really start with tests - we'll be coding as usual.

```csharp
public void Checkout(IEnumerable<int> itemIDs, int memberID, DateTime when) 
{
    var member = this.memberRepo.FindByID(memberID);
    var items = this.itemRepo.FindByIDs(itemIDs);

    var discountToApply = 0;
    var isBirthday = member.Birthday.Month == when.Month && member.Birthday.Date == when.Date;
    
    // ^ hold on, the algorithm above can be actually complex!
}
```

When there are more and more conditions coming up, we will realize that we are running into complexity. So here comes step 1.

### Declare first and test later

Instead of writing up the whole algorithm inline, we declare the algorithm as another method / function, and finish the current code block.

```csharp
public void Checkout(IEnumerable<int> itemIDs, int memberID, DateTime when) 
{
    var member = this.memberRepo.FindByID(memberID);
    var items = this.itemRepo.FindByIDs(itemIDs);

    var totalPayable = CalculateDiscountedTotal(member, items, when);

    this.paymentService.Charge(memberID, totalPayable);
}

// the algorithm
public static decimal CalculateDiscountedTotal(Member member, Item[] items, DateTime checkoutTime)
{
    // ... unimplemented
}
```

The advantage of this, is that with a complete `Checkout` method, we have confidence of the overall functionality; the decision-making of calculating discounted pricing is deferred. As a result, the `Checkout` method tells a clear story and is written in a *declarative* style.

Further more - because `CalculateDiscountedTotal` cares only about data provided through parameters, it can be made static and becomes a *pure function* - pure methods are much much easier to test. For one, it does not depend on the state of the instance - this means D.I. and mocking won't be such a big thing for unit testing.

### Drive test with data

We can now flesh out the test for `CalculateDiscountedTotal`. Good news is that we have a good idea what it should do (aka the requirements). So we start a unit test as this.

```csharp
public void If_member_is_having_birthday_Then_gets_50_percent_discount_Otherwise_none()
{
    var actual = ShoppingService.CalculateDiscountedTotal(member, items, checkoutTime);

    Assert.Equal(expected, actual); // <- ^ ah, we need member, items, checkoutTime and expected
}
```

A few things to note,

* Again I *declare* a test method by stating there will be an *actual* value and it should be equal to an *expected* value, **without** providing these values / parameters first. Why? Well it's because
    1. the focus should be on the algorithm so I want to get to it first
    2. the algorithm will tell me what data to provide. *Data-driven*, remember?

* I describe both negative and positive cases. That's because, it will be the only test method we write for this algorithm! And instead of writing out each scenario as its own test method, we will just throw all scenarios at this only test.

Now we start to create test cases.

```csharp
[Theory]
[MemberData(nameof(DiscountedPriceForMemberBirthDay_TestCases))]
public void If_member_is_having_birthday_Then_gets_50_percent_discount_Otherwise_none(Member member, Item[] items, DateTime checkoutTime, decimal expected)
{
    var actual = ShoppingService.CalculateDiscountedTotal(member, items, checkoutTime);

    Assert.Equal(expected, actual);
}

public static object[][] DiscountedPriceForMemberBirthDay_TestCases = 
new object[][]
{
    // no items <-- base case
    new object[] { new Member(), new Item[]{}, DateTime.Now, 0 }
}
```

Note here we start with a *base case*, which provides the simplest form for each parameter (you'll possibly want to pass `null` for `Member` and that's good too!). The *base case* might seem trivial but in reality it can be the most confounding test case of all! So respect to the base case :)

### Structured test data

We are now ready to throw more test cases to the test method.

```csharp
public static object[][] DiscountedPriceForMemberBirthDay_TestCases = 
new object[][]
{
    // no items <-- base case
    new object[] { new Member(), new Item[]{}, DateTime.Now, 0 },
    // having birthday
    new object[]
    {
        new Member { Birthday = DateTime.Parse("1983-04-02") },
        new { new Item { Price = 100 } },
        DateTime.Parse("2019-04-02"),
        50
    },    
    // Not having birthday
    new object[]
    {
        new Member { Birthday = DateTime.Parse("1983-04-02") },
        new { new Item { Price = 100 } },
        DateTime.Parse("2019-04-01"),
        100
    },
    // having birthday but buying no items
    // having birthday and buying multiple items
    // ...
}
```

You'll find that it now becomes a game of thinking up good combination of input and output to the method under test - and because the data-set is in one place, it makes good sense to line up our data well, for example, vary the combination by changing the fields one by one. As a result, we will get well structured test cases, and in turn it drives us to build more robust code.

## summary

Hope you'll agree with me now that T.D.D. is most practical and valuable with the points below.

1. Focus on data, decision-making and complexity, instead of I.O. or dependency injection
2. Declare high-level code first, and defer complex algorithm to pure functions
3. Pure functions are much easier to test - we need only throw data at it
4. With data-driven tests we can line up test data in a more structured and exhaustive fashion

At the end of day, I guess it comes down to this again: prefer data over code!