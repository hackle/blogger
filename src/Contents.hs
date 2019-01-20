module Contents where
    import Data.Char

    type ArticleTitle = String
    data ContentEntry = Entry { getTitle::ArticleTitle
                                , getFile:: FilePath
                                , getSlug:: String
                                } deriving Eq

    toEntry :: (ArticleTitle, FilePath) -> ContentEntry
    toEntry (artTitle, fPath) =
        Entry artTitle fPath (toSlug artTitle)
        where
            toSlug = fmap ((\c -> if c `elem` ['a'..'z'] then c else '-').toLower)

    blogContents :: [ContentEntry]
    blogContents = reverse $ toEntry <$> [
        ("How is this blog put together", "blog-architecture.md")
        -- , ("Modeling domain with F# for strong specification", "modeling-with-fsharp.md")
        , ("LINQ, infinity, laziness and oh mine!", "linq-tips.md")
        -- , ("A lens look-alike (really a nested data updater) in C#?", "lens-csharp.md")
        , ("Lens (really record viewer / updater) in TypeScript", "lens-typescript.md")
        , ("Fin", "fin.md")
        , ("Coding an alternative Vect.index, Type-Driven Development in Idris", "index-fin-alternative.md")
        , ("callCC in Haskell, and my ultimate Monad", "call-cc-my-ultimate-monad.md")
        , ("My take on (unit) testing", "my-take-on-unit-testing.md")
        , ("Serialize like javascript - MergeJSON in Idris!", "serialize-like-javascript-the-prototype.md")
        , ("Serialize like javascript - the idea", "serialize-like-javascript.md")
        , ("foldl in terms of foldr", "foldr-in-foldl.md")
        , ("Inject functions, not interfaces", "no-interface-just-use-functions.md")
        , ("Make unit testing a breeze by segregating complexity", "test-complex-keep-rest-simple.md")
        , ("Don't null check, just continue!", "dont-pattern-match-just-pass-function.md")
        ]

    about :: ContentEntry
    about = toEntry ("About Hackle's blog", "about.md")

    siteContents :: [ContentEntry]
    siteContents = about:blogContents
