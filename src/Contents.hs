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
    , ("A few things about unit testing", "presso-pragmatic-unit-testing.md")
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
    , ("the magic Const, Identity and tuple", "the-const-trickery.md")
    , ("Covariance and contravariance", "contravariant.md")
    , ("T.D.D. is most practical data-driven with pure functions", "tdd-data-driven-and-functional.md")
    , ("Nesting and positions in covariance and contravariance, ", "contravariant-positions.md")
    , ("Reducer to reduce, with lens in OO flavour", "lens-for-reducer.md")
    , ("Dependent types in TypeScript?", "dependent-types-typescript.md")
    , ("The Diamond, squashed and recovered", "the-diamond-kata.md")
    , ("Tuck-away and take-one, whatever it takes to look declarative", "anything-to-be-declarative.md")
    , ("Good code does not matter... not that much", "good-code-does-not-matter.md")
    , ("Setting CAP loose in real life", "cap.md")
    , ("Placement by functionality, not technical concerns", "where-to-place-x.md")
    , ("Plain and simple state management", "plain-state-management.md")
    , ("Self-referenced JSON?", "self-reference-json.md")
    , ("Also on Comonad and Conway's game of life", "conway-comonad.md")
    , ("Dependent Types in TypeScript, Seriously", "dependent-types-typescript-seriously.md")
    , ("Literal type preservation with TypeScript", "type-preservation.md")
    , ("A truly strongly-typed printf in TypeScript", "printf.md")
    , ("On accidental code deletion as reason for unit testing", "what-if-my-code-is-deleted.md")
    , ("The TypeScript Handbook, Optional Parameters and Postel's Law", "the-typescript-handbook-and-postels-law.md")
    ]

about :: ContentEntry
about = toEntry ("About Hackle's blog", "about.md")

siteContents :: [ContentEntry]
siteContents = about:blogContents
