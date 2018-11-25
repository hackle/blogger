In [Serialize like javascript](./serialize-like-javascript---the-idea), we figured out a way to have forward compatible serialization / deserialization without risking losing / deleting newly added fields in the schema, much like with Javascript. Without further ado, let's crack into prototyping such a design, this time, in Idris.

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

And a `format` function that does pretty printing.

```idris
format : {default 0 curr : Nat} -> (n : Nat) -> JSON -> String
```

With all above, the implementation is straightforward.

```idris
import Language.JSON
import Language.JSON.Data

replaceKey : String -> JSON -> List (String, JSON) -> List (String, JSON)
replaceKey k v xs = map tryReplace xs where
  tryReplace (k1, v1) = if k1 == k then (k, v) else (k1, v1)

mergeObjects :  (xs : List (String, JSON)) ->
                (ys : List (String, JSON)) ->
                (merger: JSON -> JSON -> JSON) ->
                List (String, JSON)
mergeObjects xs [] _ = xs
mergeObjects xs (kvp@(k, v) :: ys) merger =
  case find (\(k1, _) => k1 == k) xs of
    Nothing => mergeObjects (xs++[kvp]) ys merger
    (Just (_, v1)) => mergeObjects (replaceKey k (merger v1 v) xs) ys merger

mergeJson : JSON -> JSON -> JSON
mergeJson (JObject xs) (JObject ys) = JObject $ mergeObjects xs ys mergeJson
mergeJson _ json2 = json2

mergeJsonStrs : String -> String -> String
mergeJsonStrs x y = case (parse x, parse y) of
                      (Just a, Just b) => format 4 $ mergeJson a b
                      (_, Just b) => format 4 b
                      _ => ""
```

Because `Idris` requires anything to be declared before it's used, when reading `Idris` code, it's a good idea to start from the bottom.

* `mergeJsonStrs` merges two JSON strings, provided both are well formatted. It simply parses each string to its `JSON` (as a type in `Idris` now) representation, and merges them (when both valid) using the `mergeJson` function, and formats the result.

* `mergeJson` splits to 2 cases: if both arguments are of type `JObject`, then merge them using `mergeObjects`. Otherwise, just return the second argument. This applies to `JString`, `JBoolean`, `JNumber`, `JNull`, and notably, `JArray`.

* `mergeObjects` does the real merging, and the algorithm is simple: treat JSON as maps / key value pairs, and merge the keys from both maps, if values conflict for a key, then take the value from the second map.

Let's see if this works:

```idris
merge1 : String
merge1 = mergeJsonStrs """{
  "key1": 11,
  "key2": {
      "key2.1": true,
      "key2.2": {
        "key2.2.1": "bar",
        "key2.2.2": 200
      }
    }
  }"""
  """{
    "key1": 12,
    "key2": {
      "key2.2": {
        "key.2.2.1": "quux"
      },
      "key2.3": "foo"
    }
  }"""

main : IO ()
main = do
  putStr merge1
```

Note I needed to put it in a `main` so I can execute it in the REPL. Otherwise the output is not pretty-printed.

```Idris
*main> :exec
{
    "key1": 12,
    "key2": {
        "key2.1": true,
        "key2.2": {
            "key2.2.1": "quux",
            "key2.2.2": 200
        },
        "key2.3": "foo"
    }
}
```

Works as expected.

## Next

From here on it would be straightforward - we need to integrate this pattern to the serialization layer of our client-side applications. Depending on what frameworks / libraries are used, the integration can be of a different level of complexity, but nonetheless possible.

## Summary

A simple idea, inspired by Javascript's trivial handling of JSON objects, leads us to a simple solution in `Idris`. The solution is easily transferrable to any other language.

This kind of calls for a protocol (or an understanding) between the server and the clients. The server must promise that any schema change must be backward-compatible. Such changes are categorised by Rich Hickey beautifully as, paraphrased `requiring no more, or giving no less`.

The client side can then utilise the idea illustrated above to make sure any additive (backward-compatible) changes to the schema will be respected, kept throughout, and sent back to the server loyally.

Note this approach only applies when such loyalty is necessary, in other words, both reading and writing of data are needed for the same line of process. If only read or only write is required, then our solution is not needed, or would be overkill.

I can also see that this approach is not only applicable to JSON, but also other data formats such as XML.
