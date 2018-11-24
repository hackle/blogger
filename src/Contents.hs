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
        ("Setting up an AWS Lambda with serverless-haskell", "setup-haskell.md")
        -- , ("Modeling domain with F# for strong specification", "modeling-with-fsharp.md")
        , ("LINQ, infinity, laziness and oh mine!", "linq-tips.md")
        -- , ("A lens look-alike (really a nested data updater) in C#?", "lens-csharp.md")
        , ("Serialize like javascript PART 1: the idea", "serialize-like-javascript.md")
        , ("Lens (really record viewer / updater) in TypeScript", "lens-typescript.md")
        , ("Fin", "fin.md")
        , ("Coding an alternative Vect.index, Type-Driven Development in Idris", "index-fin-alternative.md")
        , ("callCC in Haskell, and my ultimate Monad", "call-cc-my-ultimate-monad.md")
        , ("My take on (unit) testing", "my-take-on-unit-testing.md")
        ]

    about :: ContentEntry
    about = toEntry ("About Hackle's blog", "about.md")

    siteContents :: [ContentEntry]
    siteContents = about:blogContents
