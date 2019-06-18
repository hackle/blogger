module Contents where
    import Data.Char
    import Data.List.Extra (stripSuffix, words)
    import Data.Maybe (fromMaybe)
    import Data.List (intercalate)

    type ArticleTitle = String
    data ContentEntry = Entry { getTitle::ArticleTitle
                                , getFile:: FilePath
                                , getSlug:: String
                                } deriving Eq

    toEntry :: (ArticleTitle, FilePath) -> ContentEntry
    toEntry (artTitle, fPath) = Entry artTitle fPath slug
        where
            slug = intercalate "-" $ words $ map toLower $ filter goodChar artTitle
            goodChar c = isAlphaNum c || c == ' '

    eqSlug :: String -> ContentEntry -> Bool
    eqSlug str (Entry _ path slug) = strip str == strip slug || strip str == strip pathWoExt
        where 
            strip str = toLower <$> filter isLetter str
            pathWoExt = fromMaybe path $ stripSuffix ".md" path

    blogContents :: [ContentEntry]
    blogContents = reverse $ toEntry <$> [
        ("How is this blog put together", "blog-architecture.md")
        ,("A few things about unit testing", "presso-pragmatic-unit-testing.md")
        -- , ("Modeling domain with F# for strong specification", "modeling-with-fsharp.md")
        , ("LINQ, infinity, laziness and oh my!", "linq-tips.md")
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
        , ("2-layer architecture", "two-layer-architecture.md")
        , ("Types and tests: JavaScript 10, Idris 0", "types-and-tests.md")
        , ("Types, names, and type superstition", "type-superstition.md")
        , ("Out-of-context string template is an anti-pattern", "out-of-context-string-template.md")
        , ("Good code does not matter... not that much", "good-code-does-not-matter.md")
        , ("the magic Const, Identity and tuple", "the-const-trickery.md")
        , ("Covariant and contravariant from C# to Haskell to C# to Haskell", "contravariant.md")
        , ("T.D.D. is most practical data-driven with pure functions", "tdd-data-driven-and-functional.md")
        ]

    about :: ContentEntry
    about = toEntry ("About Hackle's blog", "about.md")

    siteContents :: [ContentEntry]
    siteContents = about:blogContents
