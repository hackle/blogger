In [Serialize like javascript](./serialize-like-javascript), we figured out a way to have forward compatible serialization / deserialization without risking losing / deleting newly added fields in the schema, much like with Javascript. Without further ado, let's crack into prototyping such a design, this time, in Idris.

## merge JSON strings

I reckon we can get a good start if we can merge two well formatted JSON strings, a simple example (from the previous post):

```idris
 mergeJsonstrs """{ "name": "george", "age": 11, "address": "thunderbold street" }""" """{ "name": "george", "age": 12 }"""

 -- """{ "name": "george", "age": 12, "address": "thunderbold street" }"""
```

I will use the `Language.JSON` package. In `Atom`, I added a new file `myProject.ipkg` to enable importing from this package. Its content is simple:

```idris
pkgs = contrib
```

`contrib` is just where the package is defined, as in [here](https://github.com/idris-lang/Idris-dev/blob/master/libs/contrib/Language/JSON.idr)

There is already `JSON` type which is defined as:

```Idris
data JSON
   = JNull
   | JBoolean Bool
   | JNumber Double
   | JString String
   | JArray (List JSON)
   | JObject (List (String, JSON))
```

There is a `parse` function as
```idris
parse : String -> Maybe JSON
```

And a `format` function that DOES pretty printing.

```idris
format : {default 0 curr : Nat} -> (n : Nat) -> JSON -> String
```

With all above, the implementation is straightforward.
