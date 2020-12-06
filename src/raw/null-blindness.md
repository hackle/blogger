"null blindless" is just as bad as the famous "boolean blindness", and possibly more widespread.

Have you heard of people's mishap for having "null" in their name, such as that of [Jennifer Null](https://www.bbc.com/future/article/20160325-the-names-that-break-computer-systems).

> When Jennifer Null tries to buy a plane ticket, she gets an error message on most websites. The site will say she has left the surname field blank and ask her to try again.

And the reason might not be as simple as type coercion, e.g. `null == "null"` which is pretty rare. It's pretty close to one of the scenarios, something like `<Element>null</Element>` as explained [here](https://stackoverflow.com/questions/4456438/how-to-pass-null-a-real-surname-to-a-soap-web-service-in-actionscript-3).


So the question comes down to: are you someone identified as `null`, or do you not exist?

This may sound like nonsensical. Now think about this: every time you write `let foo = null`, do you mean `foo` does not have any value, or it has a value of `null`?

If it has a value of `null`, how can I express `foo` without a value?

Let me give you an example

```
[ { score: "foo" }, { firstName: null }, { firstName: "bar" } ]
```

There might be no difference, or the difference is negligible.

Does it exist, or is it just a null value?

REST: keep it, or delete it?
Yet another dilemma is when designing a REST method. For example, a `member` entity is defined as

```
{ firstName: "Foo", lastName: "Bar" }
```

And we want to update the `firstName`, but keep `lastName` as is.

```
PATCH /member/23
{ "firstName": "Baz" }
```

A REST guru would have noted for update the `firstName`, one could use `PUT /member/23/firstName`; to delete `lastName`, `DELETE /member/23/lastName`. This could be quite non-trivial to implement, even with a help of a good REST framework.

The solution

Introduce `undefined` to represent **non-existence**

Create a new type

Use a language with better language

Express intent with data / web API design


The joke is on us, software peeps. I know most of us will think, this would NEVER happen on my watch! Maybe not quite, if you are not using a dynamic language daily (oh you look a bit pale, front-end developers). Even if you don't, devious as `null` is, we'll have to be extremely careful to not let one slip through.
