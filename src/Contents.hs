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
        , ("Linq first, Linq always", "linq-revisited.md")
        -- , ("A lens look-alike (really a nested data updater) in C#?", "lens-csharp.md")
        , ("An idea: data-preserving JSON deserializer", "no-loss-serializer.md")
        , ("Lens (really record viewer / updater) in TypeScript", "lens-typescript.md")
        , ("Fin", "fin.md")
        , ("Coding an alternative Vect.index, Type-Driven Development in Idris", "index-fin-alternative.md")
        ]

    about :: ContentEntry
    about = toEntry ("About Hackle's blog", "about.md")

    siteContents :: [ContentEntry]
    siteContents = about:blogContents
