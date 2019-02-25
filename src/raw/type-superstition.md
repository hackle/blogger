"Names are not important, types are", so have I heard on more than one occasion, spoken for programming with static typing. Granted it is just another attention-grabbing blanket statement, however I do recognize the strong intention of over-correction for under-utilization of types in main-stream programming. 

Names or types? What really matters is whether code expresses its purpose well. The problems usually falls into two kinds: **over-specification**, when names / types repeat each other in describing the same thing, or **under-specification**, when names / types do not express the purpose clearly enough. Let's see what they look like respectively.

The astute reader would have called out already that types also have names, so this names vs types argument is non-existent. However, for the purpose of the discussion at hand, we'll loosely refer to **types** as data types, either primitive, complex, or user defined; **names** as the names we give values, variables or functions. (Or possibly classes, but classes can be both types and names. It is further proof that this post is not to be taken too seriously in a academic light)

## Over-specification

You'll see over-specification happens more often with Object Oriented Programming, ironically with the cliche "naming is hard". This is no coincidence - let's see why.

### naming Class + Method + Parameters

Popular teaching such as SOLID plus naming conventions (such as to name classes with nouns, and methods with verbs), usually lead to code as below:

```csharp
interface MemberService
{
    void SaveMember(Member member);

    // other methods
}
```

Alas, `Member` is specified 4 times, respectively in the interface name, the method name, the parameter type, and the parameter name.

While certainly not harmful, man this is repetitious! It's like **"It's deja vu all over again."** but twice as bad. We can certainly be more succinct. For a start, I would do

```csharp
interface MemberService
{
    void Save(Member m);
}
```

Because the type `Member` provides enough information, I don't mind naming the parameter with a very minimalistic `m`. Even so, it still feels redundant, it would be ideal if we don't need to give it a name. And actually, there is!

```csharp
interface MemberService
{
    Action<Member> Save;
}
```

(You may find the use of `Action` a bit exotic, but using `Action` or `Func` actually offers more flexibility than methods, for example, they can be changed at runtime).

With that, `Save` is reduced to something like `Member -> void`, which expresses its input and output (side-effect) adequately. See, names does not matter, sometimes they can be removed without much loss!

This is hardly surprising. Well, types have names too, so they can express themselves just fine! 

The catch is then - we need to name types well too. For example, if instead of `Member` we use `M`, it would be hard to guess its meaning. Apparently, the more specific the type the better.

Naturally, this leads us to types with very broad meanings - primitive types.

## Under specification

Primitive types offers very little meanings on their own, so naming values with such types becomes more important.

Consider:

```haskell
parseCode :: String -> Bool -> HTML
```

Believe it or not, this is a real-world example from a library I recently came across. This signature failed me completely, so I had to dig into the implementation to find out what `parseCode` really does: it translates a markdown code block to HTML. The code block can be fenced (indented, loosely speaking).

Apparently types like `String` and `Bool` are not helping here - they are too broad to offer any specific meaning. A common technique to improve this, is to use type alias, such as

```haskell
type Code = String
type IsFenced = Bool

parseCode :: Code -> IsFenced -> HTML
```

Using type alias is usually very lightweight as there is no need of boxing and unboxing. The small inconvenience is that users will first need to learn what `IsFenced` / `Code` really are with an extra lookup.

This is where naming becomes useful, consider below code in TypeScript:

```typescript
type HTML = string

function parseCode(code: String, isFenced: boolean): HTML {
    return "<div>yoneda</div>";
}
```

Where types fail, names shine. 

You would have noticed that I also used a type alias for `HTML`, a nice feature of TypeScript. A small shame is when I try to use intellisense to inspect the type of `parseCode`, return type `HTML` is collapsed to `string`. I guess the upside is callers won't have to take an extra step to look up what `HTML` type really is.

There are times even the best names can't help. For example, when we have parameters of the same primitive types together, we can get callers in trouble.

```csharp
int CalculateDiscount(int customerId, int orderId, int promoId)
```

Now the poor caller can mix up the order of `customerId`, `orderId` and `promoId`, the compiler won't care as the types are satisfied, so chances are the user will see strange errors at runtime. Naming them well won't really help much, not even making a type alias for each parameter.

Rather than arguing the user is at fault for not respecting the names of the parameters, we can create new types to make the mix-up impossible.

For example, in C# we can create separate classes / structs.

```csharp
class CustomerId { public int Id { get; set; } }
class OrderId { public int OrderId { get; set; } }
class PromoId { public int PromoId { get; set; } }
```

So `CalculateDiscount` becomes

```csharp
int CalculateDiscount(CustomerId cId, OrderId oId, PromoId pId)
```

The caller now has enough context and is less likely to mistake one parameter for another.

However, if equality is required, which is only reasonable, we'll need to override `Equals` and `GetHashCode`. A chore. Possibly why I don't see this done very often, despite the obvious benefits and strongly advocated by Domain Driven Design.

In Haskell, we would create new types.

```haskell
newtype CustomerId = CustomerId Int deriving Eq
newtype OrderId = OrderId Int deriving Eq
newtype PromoId = PromoId Int deriving Eq

calculateDiscount :: CustomerId -> OrderId -> PromoId -> Int
```

Note how cheap it is to create new types with derived equality. The trade-off is now we need to box and unbox for values in these types.

## just right

Excessive emphasis on the expressive power of types is often spoken by programmers using languages with strong type systems such as Haskell. When the emphasis is taken to the extreme, I refer to it as **Type Superstition**. 

Haskell's type system is amazing. As an example, I use [Hoogle](https://www.haskell.org/hoogle/) a lot to search for documentation of functions. Amazingly, search on Hoogle is purely type based, which tells us how expressive types can be. But does that mean names are not important in Haskell? I wouldn't say so.

One good example is `reverse`, which has the type of `reverse :: [a] -> [a]`. If we just search by `[a] -> [a]`, there is a host of functions with this type, such as `cycle`, `init` and `tail`. They each have different behaviours and are not to be mixed up.

If we are not on the extreme of programming in dynamic languages where static typing is not available and names overwhelmingly important, with popular statically typed languages, in practice, we need both names and types, with the right combination, to write expressive code.