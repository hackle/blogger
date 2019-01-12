It was not easy for me to learn and come to terms with mocking, and soon after I did, I realised my unit tests (and code under test) are better off without mocking - and even dependency injection.

Here in this post we look at how simple it is to do away with mocking, by, you guessed it, using functions, 

## a typical example in C#

Let's say we have a `PaymentService` that goes

```csharp
class PaymentService
{
    PaymentService(IUserBalance userBalance)
    {
        ... initialise fields with arguments
    }

    public void Pay(decimal amount, int userId)
    {
        var balance = userBalance.GetByUserId(userId);
        if (balance < amount)
        {
            throw new NotEnoughBalance();
        }

        // todo: much more validation here :)

        userBalance.Set(userId, balance - amount);
    }
}
```
(Please forgive me for lack of concurrency support.)

This service is typically used in a controller as

```csharp
class PaymentController
{
    readonly PaymentService;

    public void Pay(decimal amount)
    {
        PaymentService.Pay(amount, GetUserIdFromContext());
    }
}
```

## test with mocks

Typically we need 2 unit tests for `PaymentService.Pay()` (again forgive me for not test driving the service) as it has a `Cyclomatic complexity` of 2. In other words there are 2 scenarios to cover: when balance is less than amount to pay, and when balance is no less.

Just to show that I do know how to use mocking, here is how the tests can look like:

## don't use interface, just use functions
`IUserBalance`, like most interfaces created for the sake of unit testing and dependency injection, is a typical [header interface](https://martinfowler.com/bliki/HeaderInterface.html). I am not a fan of such interfaces, and would like to see them removed - less code is better than more.

## now test with functions!

## Summary
By using functions, we can unit test without mocking. There are much to be said about mocking but regarding the code under test in this post, I am happy to see the removal of dependency on a mocking framework while keeping the unit tests straighforward.

I believe in minimalism in dependencies 