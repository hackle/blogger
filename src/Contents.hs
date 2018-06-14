module Contents where
    import Data.Char

    type ArticleTitle = String
    data ContentEntry = Entry { getTitle::ArticleTitle
                                , getFile:: FilePath
                                , getSlug:: String
                                }

    toEntry :: (ArticleTitle, FilePath) -> ContentEntry
    toEntry (artTitle, fPath) =
        Entry artTitle fPath (toSlug artTitle)
        where
            toSlug = fmap ((\c -> if c `elem` ['a'..'z'] then c else '-').toLower)

    blogContents :: [ContentEntry]
    blogContents = reverse $ toEntry <$> [
        ("Setting up an AWS Lambda with serverless-haskell", "setup-haskell.md")
        , ("Modeling domain with F# for strong specification", "modeling-with-fsharp.md")
        , ("Linq revisited", "linq-revisited.md")
        , ("Lens in C#, really?", "lens-csharp.md")
        , ("Lens in TypeScript", "lens-typescript.md")
        ]

    siteContents :: [ContentEntry]
    siteContents = (blogContents ++) $ toEntry <$> [
        ("About hackman", "about.md")
        ]