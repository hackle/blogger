I have Javascript envy when it comes to consuming JSON, because it needs no serialization / deserialization. This is not the case for most languages, which usually require some sort of serialization, and such operations are usually lossy and may result in unintended effects.

We try to explore a way of serialization / deserialization so this statement can hold: when API / data schema is concerned, **if the server can promise to be backward-compatible, then it's possible to build the client side application to be forward-compatible, just like that in Javascript.**

(This post is inspired by Rich Hickey's talk [Spec-ulation](https://youtu.be/oyLBGkS5ICk), as unrelated as it may seem)

Let's say we want to build an awesome app called ``plus1`` whose job is to make any user one year older.


## `plus1` in Javascript

* read the existing profile of the user from a web API, e.g. in a controller

```javascript
httpClient.get('api/user/123.json')
	.then(user => this.user = user);

// http returns
GET api/user/123.json
{
	name: 'George',
	age: 11
}
```

* now the `user` object can be modified immediately (without any form of serialization).

```javascript
	// later in the controller
	this.user.age = this.user.age + 1;
```

* and we send the modified profile back to the API (without any form of deserialization)

```javascript
httpClient.put('api/user/123.json', this.user)
	.then(response => celebrateWith10Toasters('success!'));

// HTTP 200 OK
```

Pretty simple right? However, things are not quite as intuitive in some other languages

## the same application in C# as an example

* read the existing profile of the user from an API.

```csharp
var response = httpClient.Get("api/user/123.json");
```

* `response` is still a `string`, so we cannot use it directly in our strongly-typed business domain yet. Instead we need to deserialize the response to a typed object e.g. `User`.

```csharp
var user = Deserialize<User>(response);

// User is defined as
class User
{
	String Name;
	Int Age;
}
```

* now the `user` object can be modified.

```csharp
user.Age = user.Age + 1;
```

* when putting the changes made to `user` to the API, we cannot send the `User` object directly because a typical `httpClient` requires a `string` as payload. So we need to serialize the modified `user` to a string.

```csharp
var payload = Serialize<User>(user);
```

* eventually we send the payload to the API

```csharp
httpClient.Put("api/user/123.json", payload);
// HTTP 200 OK
```

Two extra steps for serialization and deserialization, not great, but not the end of the world either.

## when it becomes not so great

Assume the schema of user profile changes, an `address` is now added.

Note this is not a breaking change (which means the server either requires more, or gives less)

```javascript
GET api/user/123.json
{
 name: "George",
 age: 11,
 address: "thunderbolt street" // <-- new
}
```

Our `plus1` app in `javascript` will have no problem dealing with this, as the new field will simply flow all the way through to the time we send the modified `user` back to the API. In fact, nothing needs to be done!

The `plus1` in `C#` though, will now have a problem...

* as the `User` class has no knowledge of the newly added `address`, when deserialized, the `address` field is lost
* later, when the modified `user` is serialized and posted to the API, the `address` field is missing. Very likely, as a result, the user's address will be deleted!

This presents the versioning problem - limited by serialization / deserialization, client side can only deal with the old version / schema, and will result in unintended deletion when dealing with the new version.

One way to work around this, is to demand the server not change the schema of the API response at all. Instead, create a new version of API and serve it separately through a different URI, e.g. `api/user/v2/123.json`. Client-side applications can switch to the new version whenever it is ready.

This is a sound solution. However, the server side can rightfully argue that such changes are not breaking changes, so there is no need for versioning. Indeed, if we version each of such non-breaking changes, and if the API is a fast evolving (and popular) one, the client-side applications will have a hard time (if ever) catching up with the latest version. And before we know it, we are on `v30` and chances are, many (if not all) of these versions need to be maintained!

## a more robust solution

Think about it, if the client-side application has been written in Javascript, we would never have such a headache (even though we might have others) - new fields can be freely introduced and they will freely flow through (and back), never to be lost in translation. Not even a single line of code needs changing at the client side to deal with this!

Is the moral, then, that we should all desert our beloved programming language and use Javascript only, so that we can have free-flowing JSON? Tempting... but I for one am not sure about giving up the likes of Idris, Haskell and F#, yet, not even C#.

But how do we make up for the loss in serialization / deserialization? Well, it's simple, we just keep JSON on the side!

* when deserialize a string to a `User`, keep the original string with the `User` object. so `User` can now be defined as:

```csharp
// user is defined as
class User
{
	String Name;
	Int Age;
	String OriginalJsonString;
}
```

(I am going for simplicity here, you don't have to make your implementation this crude. By all means, go for generics or reflection)

* when serialize a `User` back to a JSON string, before we send it off to the API, merge the modified `User` into the original JSON string, and then send off the combined string, which will have the `address` field intact. In psuedo-code:

```csharp
string modified = Serialize<User>(modifiedUser);
string merged = MergeJSON(modifiedUser.OriginalJsonString, modified);

// OriginalJsonString {  name: "George", age: 11, address: "thunderbolt street" }
// serialized: { name: "George", age: 12 }
// combined: { name: "George", age: 12, address: "thunderbolt street" }

httpClient.Post("api/user/123.json", merged); // nothing is lost!
```

That's the idea! Wouldn't you agree that it's really really simple?

Now you may be wondering how we can implement this `MergeJSON` method - honestly I am not really worried but as a developer I do enjoy getting my hands dirty, so I will soon get my hands dirty in writing `MergeJSON` in `Idris`. (edite: and it's right here [MergeJSON in Idris](./serialize-like-javascript---mergejson-in-idris-)))

In the mean time, you might have better luck finding that it's [already there](https://www.newtonsoft.com/json/help/html/MergeJson.htm) or [there](https://stackoverflow.com/questions/9895041/merging-two-json-documents-using-jackson) for free!
