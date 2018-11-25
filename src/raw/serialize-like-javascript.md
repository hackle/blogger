I have Javascript envy when it comes to consuming JSON, because it needs no serialization / deserialization. This is not the case for mostly languages, which usually requires some sort of serialization and such operations are usually lossy and may result in unintended effects.

We try to explore a way of serialization / deserialization so this statement can hold: when API / data schema is concerned, **if the server can promise to be backward-compatible, then it's possible to build the client side application to be forward-compatible, just like that in Javascript.**

(This post is inspired by Rich Hickey's talk [Spec-ulation](https://youtu.be/oyLBGkS5ICk) as unrelated as it may seem)

Let's say we want to build an awesome app called ``plus1`` whose jobs is to make any user one year older.


## `plus1` in Javascript

1. read the existing profile of the user from a web API, e.g. in a controller

		httpClient.get('api/user/123.json')
			.then(user => this.user = user);

		// http returns
		GET api/user/123.json
		{
			name: 'George',
			age: 11
		}

2. now the `user` object can be modified immediately (without any form of serialization).

		// later in the controller
		this.user.age = this.user.age + 1;

3. and we send the modified profile back to the API (without any form of deserialization)

		httpClient.put('api/user/123.json', this.user)
			.then(response => celebrateWith10Toasters('success!'));

		// HTTP 200 OK

Everybody happy.

Things are not quite as intuitive in other languages

## the same application in C# as an example

1. read the existing profile of the user from an API.

		var response = httpClient.get('api/user/123.json');

2. deserialize the response to a value

		var user = Deserialize<User>(response);

		// User is defined as
		class User
		{
			String Name;
			Int Age;
		}

3. now the `user` object can be modified.

		user.Age = user.Age + 1;

4. before posting it to the API, we need to serialize the modified `user` to a string.

		var payload = Serialize<User>(user);

5. eventually we send the payload to the API

		httpClient.put('api/user/123.json', payload);
		// HTTP 200 OK

Two extra steps for serialization and deserialization, not great, but not the end of the world either.

## when it becomes not so great

Assume new information is added to the user profile, say `address`.

```javascript
GET api/user/123.json
{
 name: 'George',
 age: 11,
 address: 'thunderbolt street' // <-- new
}
```

Our javascript version of `plus1` will have no problem dealing with this, as the new field will simply flow all the way through to the time we send the modified `user` back to the API.

The other version though, will now have a problem...

* as the `User` class has no knowledge of the newly added `address`, when deserialized, the `address` field is lost
* later, when the modified `user` is serialized and posted to the API, the `address` field is missing, as a result, the user's address will be deleted!

This presents the versioning problem - limited by serialization / deserialization, client side can only deal with the old version / schema, and will result in unintended deletion when dealing with the new version.

One way to work around this, is to not change the schema of the API response at all. Instead, a new version of API is created and served separately through a different URI, e.g. `api/user/v2/123.json`. Client-side applications can switch to the new version whenever it is ready.

This is a sound solution. However, if the API is a fast evolving (and popular) one, the client-side applications will have a hard time (if ever) catching up with the latest version. And before we know it, we are on `v30` and many (if not all) previous versions need to be maintained!

## a more robust solution

Think about it, if the client-side application has been written in Javascript, we would never have such a headache (even though we might have others) - new fields can be freely introduced and they will freely flow through (and back), never to be lost in translation. Not even a single line of code needs changing at the client side to deal with this!

Is the moral, then, that we should all desert our beloved programming language and use Javascript only, so that we can have free-flowing JSON? Tempting... but I for one am not sure about giving up the likes of Idris, Haskell and F#, yet, not even C#.

But how do we make up for the loss in serialization / deserialization? Well, if we can't beat them, join them! We can simply keep JSON on the side!

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
* when serialize a `User` back to a JSON string, before we send it off to the API, merge the modified `User` into the original JSON string, and then send off the combined string, which will have the `address` field intact. In psuedo-code:

```csharp
string modified = Serialize<User>(modifiedUser);
string combined = MergeJSON(modifiedUser.OriginalJsonString, modified);

// OriginalJsonString {  name: 'George', age: 11, address: 'thunderbolt street' }
// serialized: { name: 'George', age: 12 }
// combined: { name: 'George', age: 12, address: 'thunderbolt street' }

httpClient.post('api/user/123.json', combined); // nothing is lost!
```

Problem solved?! Almost, but not quite. We need to introduce this pattern to the serialization layer of our client-side applications; more importantly, we need to find a `MergeJSON` function in the language we use.

Despair not - the worst part is over and the rest is what we as developers are good at. I will soon get my hands dirty in writing `MergeJSON` in `Idris`.

In the mean time, you might have better luck finding that it's [already there](https://www.newtonsoft.com/json/help/html/MergeJson.htm) or [there](https://stackoverflow.com/questions/9895041/merging-two-json-documents-using-jackson) for free!

(Also see this [prototype in Idris](./serialize-like-javascript-the-prototype))
