By strategically keeping I/O in one layer and (pure) domain logic in another, we get an application architecture that's easy to reason with and straight-forward to test as a whole.

This is a continuation of previous posts [Inject functions, not interfaces](inject-functions--not-interfaces) and [Make unit testing a breeze by segregating complexity
](make-unit-testing-a-breeze-by-segregating-complexity).

## example

As an example let's say we want to change password for a user through a `UserAccountController`. It would be written as follows.

```csharp
public class UserAccountController 
{
    private readonly IUserAccountService userAccountService;

    public UserAccountController(IUserAccountService userAccountService)
    {
        this.userAccountService = userAccountService;
    }

    [HttpPost]
    public void ChangePassword(ChangePasswordRequest request)
    {
        this.userAccountService.ChangePassword(request.OldPassword, request.NewPassword, this.UserId);
    }
}

public class UserAccountService : IUserAccountService
{
    private readonly IUserAccountRepository userAccountRepository;
    private readonly IUserAccountValidator userAccountValidator;
    private readonly IPasswordValidator passwordValidator;

    public UserAccountService(IUserAccountRepository userAccountRepository,
        IUserAccountValidator userAccountValidator,
        IPasswordValidator passwordValidator)
    {
        this.userAccountRepository = userAccountRepository;
        this.userAccountValidator = userAccountValidator;
        this.passwordValidator = passwordValidator;
    }
    public void ChangePassword(string oldPassword, string newPassword, int userId) 
    {
        var userAccount = this.userAccountRepository.GetById(userId);

        // any validator can throw exception if validation fails
        this.userAccountValidator.IsEnabled(userAccount);
        this.userAccountValidator.PasswordMatches(userAccount, oldPassword);
        this.passwordValidator.IsStrongEnough(newPassword);

        this.userAccountRepository.SetPassword(newPassword, userId);
    }
}

public class UserAccountRepository : IUserAccountRepository
{
    public UserAccount GetById(int userId)
    {
        // makes call to database
    }

    public void SetPassword(string password, int userId)
    {
        // similarly, makes call to database
    }
}
```

This design will hopefully look familiar to many of us, it is popularly referred to as **onion architecture** as it consists of layers around layers (it can be n-layer, not necessarily 3).

- the `UserAccountController` lives in the application layer, typically using frameworks such as `ASP.NET MVC`. This layer takes care of cross-cutting concerns such as authentication, authorization, HTTP, marshalling etc. The controller uses `IUserAccountService` in the domain layer. 

- the `UserAccountService` lives in the domain layer, where all the business logic takes place. The service uses `IUserAccountRepository` in the DB layer.

- the `UserAccountRepository` lives in the DB layer where interaction with database takes place.

The upstream layer uses the downstream layer through interfaces, and implementations are wired up to interfaces through **Dependency Injectors**.

A side note: I consider layering to be conceptual rather than physical. In other words, the controller, service and repository do not have to live in separate projects / assemblies, they can be under the same folder or namespace. In fact, I prefer grouping code by product features over technical characteristics. To be discussed separately.

## try testing the whole feature

Suppose now I want to test the `Change Password` feature as a whole - the controller, service, validators, repositories all together. Compared to testing each component separately, this would obviously give us more confidence on the entire feature. 

Now believe it or not, this is actually not easy to do. 

One option is to use end-to-end tests, which usually involves getting the application and all its dependencies up and running. Think web server, database and data for user accounts, session management, external API dependencies, logging services, etc, etc. This would give us the confidence we need, but from experience such tests are quite hard to set up, slow to run, hard to get right and usually fragile therefore hard to maintain. In other words, they are too expensive. (However they do hold their places and can be really valuable when the cost is justified).

If we want faster feedback and more robust tests, a good alternative would be to test the feature without the fragile parts such as external dependencies or I/O. This would make the tests a lot like unit testing on steroid. This is what we will do.

With an onion architecture and heavy use of dependency injection, this becomes quite awkward to do. I/O and side-effect are deeply nested and hidden in unknown places, and we will need to new up the whole tree of dependencies from controller to repository, occasionally mocking components.

We need to change things up. Let's first see what happens if we elevate all I/O operations all the way up to the controller's layer.

## elevate I/O to the surface

There are two steps to this: simplifying operations without I/O and elevating those with.

### simplifying operations without I/O

For any operations without I/O, we convert them to static methods.

For our example, the validators depend only on the parameters passed in (presumed), so they can be converted to static methods as described in [this post](make-unit-testing-a-breeze-by-segregating-complexity). For example, the `PasswordValidator` can be converted to:

```csharp
PasswordValidator.IsStrongEnough(newPassword);
```

Same goes for `UserAccountValidator`. Thus we removed the ceremony of dependency injection for them, as well as the need to mock these classes for testing.

### elevating operations with I/O

Using the technique described in [this post](inject-functions--not-interfaces), we replace the interfaces with functions passed in as parameters to each method. This in turn makes the containing class good candidate as a static class.

Enough talking. This is easier done than said. And `UserAccountService.ChangePassword` now looks like:

```csharp
public static void ChangePassword(string oldPassword, 
    string newPassword, 
    int userId,
    Func<int, UserAccount> getUserAccountById,
    Action<string, int> setPasswordForUserId) 
{
    var userAccount = getUserAccountById(userId);

    // any validator can throw exception if validation fails
    UserAccountValidator.IsEnabled(userAccount);
    UserAccountValidator.PasswordMatches(userAccount, oldPassword);
    PasswordValidator.IsStrongEnough(newPassword);

    setPasswordForUserId(newPassword, userId);
}
```

Cleaner? Now `UserAccountController.ChangePassword` can be made more straight-forward as well:

```csharp
[HttpPost]
public void ChangePassword(string oldPassword, string newPassword)
{
    var userAccountRepository = new UserAccountRepository();
    UserAccountService.ChangePassword(oldPassword, 
        newPassword, 
        this.UserId,
        userAccountRepository.GetById,
        userAccountRepository.SetPassword);
}
```

Voila. The **Change Password** feature is now implemented in two layers.

* the I/O layer in which the controller calls the `UserAccountService`, passing in I/O functions to read from and write to database. 
* the business layer, where `UserAccountService` lives, is now free of I/O, and therefore pure

And don't freak out when you see `UserAccountRepository` new'ed up in stark day light - it's OK! Read on.

Let's see how we can test the feature as a whole.

## testing of two-layer design

We've elevated all I/O to the controller level, but we have one last problem - `ChangePassword` calls I/O in `UserAccountRepository` directly, that's not easy to test. We need to change it still a little more. Options are to create a companion / extension class to the controller, or use D.I. for the controller (but controller only). To keep it simple, here we simply create a static overload of `ChangePassword`, for the current non-static version to call through.

```csharp
[HttpPost]
public void ChangePassword(ChangePasswordRequest request)
{
    var userAccountRepository = new UserAccountRepository();
    ChangePassword(request, 
        this.User.Id,
        userAccountRepository.GetById,
        userAccountRepository.SetPassword);
}

// NOTE this is static not hooked up to [HttpPost]
public static void ChangePassword(ChangePasswordRequest request,
    int userId,
    Func<int, UserAccount> getUserAccountById,
    Action<string, int> setPasswordForUserId)
{
    UserAccountService.ChangePassword(request.OldPassword, 
        request.NewPassword, 
        userId,
        getUserAccountById,
        setPasswordForUserId);
}
```

What's important here is to

- keep the non-static version an `integrator` method that only integrates I/O or side-effects such as database interaction (`UserAccountRepository`), sessions data (`User.Id`), marshalling request / response (`ChangePasswordRequest`), Auth (such as `[HttpPost]`) etc.
- the static version handles any decision-making (although it should be pushed to the business layer as much as possible) and call through to the underlying business layer.

And what's left to do is to test the static version of `UserAccountController.ChangePassword`. As an example, 

```csharp
[Fact]
public void If_old_password_does_not_match_Then_throws_BadOldPasswordError()
{
    var request = new ChangePasswordRequest { OldPassword = "foo" };
    var userId = 1234;

    Assert.Throws<BadOldPasswordError>(() =>   
        UserAccountController.ChangePassword(request,
            userId,
            _ => new UserAccount { Password = "bar", Enabled = true },
            (pwd, uId) => { return; }));
}
```

I hope you'll find this easy enough to do - there is no mocking, test data is passed in through lambda, which is very light-weight and cheap.

What is now made possible, is that this test validates that the `UserAccountController`, the `UserAccountService` and `UserAccountValidator` as well as other dependencies work together as expected. The scenario of password mismatch is validated loyally against the execution path as would be in production.

Also we work from the application layer directly, so there is no reason why we cannot wire up serialization / deserialization to validate requests / responses in JSON format, or even make part of continuous integration to prevent changes that break existing contracts. Sounds familiar?

## caveat

- For complex features that integrate a lot of I/O operations, passing each operation down as a separate parameter can be a bit unwieldy. One solution is to create an object to hold all the operations, and pass them down as one parameter. (this appears a bit like the `Reader` monad).

- As much as I love the high level of confidence from such "integration" tests, I would advise keeping the amount in moderation, such that is enough to cover the happy paths and key scenarios.

Reason being if the domain is complex enough, it would be very difficult to keep up with all possible scenarios from the application layers. Instead, it would be much more pragmatic to rely on unit testing for more thorough code coverage.

## inspiration

This is another post that's inspired by pure functional languages such as Haskell, in which applications are usually designed in a way that I/O is kept at the surface level, and the rest of the application, usually core domain logic, is free of I/O and side effects.
