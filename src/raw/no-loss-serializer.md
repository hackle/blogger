## What I want - the specification
What if we have a pair of serialize and deserialize functions that are isomorphic, or in this case, that are totally complementary.

```haskell
serialize . deserialize == id
-- also
deserialize . serialize == id
```
so that

```haskell
str == serialize (deserialize str)
```
That is, **EVEN IF** str has extra information that's not deserialize-able!

Of course also this - already true for most serializers
```
object == deserialize (serialize object)
```
## What I get
An ordinary deserialize function does not preserve information that's extra to the given schema. Consider

```C#
void Main()
{	
	var source = "{\"Title\": \"Bar\", \"Age\": 21 }";

	Console.WriteLine(JsonConvert.SerializeObject(JsonConvert.DeserializeObject<Foo>(source)) == source);
    // not true, "Age" field is lost because it's not a property of Foo
}

public class Foo { public string Title { get; set; } }
```

## Why is that a problem?

Usually this is not a problem at all - and how could deserialize preserve what it has no knowledge of (in this case, the **Age** field)?

However, consider this contrived example

1. **Foo** is a type of resource exposed through API **/api/foos**
2. To update a **Foo**, the consumer  
3. first reads a **Foo** with **/api/foos/321.json** as a JSON string
4. deserializes it, and modifies it
5. and serializes it again to a JSON string, and HTTP PUT the request to **/api/foos/321.json**

Now suppose the schema of **Foo** changes on the API, say from one field only 
```
{ Title: "Bar" }
```
to two fields 
```
{ Title: "Bar", Age: 21 }
```
Without knowing this change or having not updated the schema on the client side accordingly, the above consumer will happily carry on as if there is no **Age** field at all. So at step **4**, **Age** field is lost, and at step **5**, a request with no **Age** field is sent to the API, who may think the consumer intends to delete **Age** from this **Foo**! (There are workarounds to this such as versioning or treating lack of value as action required, like some kind of PATCH).

Wouldn't it be pretty handy if at step **4**, the **Age** field is carried along with the deserialized object **Foo** (although there is no **Age** field on type **Foo** at all!), therefore at step **5**, it's sent to the API, surviving serialize + deserialize?!

## How could this be done?

One option is to keep a registry of the original strings as returned from the API. When deserializing, keep any fields (in this case, the **Age** field) that are not parsed or used; then at serialization time, append the fields back, voila - **Age** field is preserved!

This would require a global registry, which needs to keep track of all objects and match them for serialization - a bit too stateful to be elegant.

Ideally, if the original string is kept with the deserialized **Foo** object, it becomes much much easier to manage.

Even better, if we can keep the JSON string up to date, namely, reflect the state of the **Foo** object, then serialization becomes a non-issue - we can just send the attached string directly to the API.

## Implementation - an example

By now it must be obvious where this is going - the DTO to use must have pretty smart getters and setters, who knows how to read from and propagate changes to an underlying string. In C#, I smell a bit of code generation; in Haskell, smart lenses.

This is still an idea and I have not implemented any of it - maybe someone will shout "something out there already does this", then I'll go "hurrah!!!" and happily use whatever that is. Otherwise I will definitely try this out with at least a proof of concept. Join me if you like this idea too!