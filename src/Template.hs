{-# LANGUAGE OverloadedStrings #-}

module Template where

import Text.Blaze.Html5
import qualified Text.Blaze.Html5 as H
import Text.Blaze.Html5.Attributes as A

type HtmlBody = H.Html

fromTemplate :: AttributeValue -> HtmlBody -> H.Html -> H.Html
fromTemplate base body styles = H.docTypeHtml $ do
    H.head $ do
        H.title "Hackle's blog"
        H.base ! href base
        H.link ! rel "stylesheet" ! href "https://cdnjs.cloudflare.com/ajax/libs/github-markdown-css/2.10.0/github-markdown.min.css"
        H.style styles 
    H.body ! class_ "markdown-body" $ do
        H.header $ do
            H.a ! class_ "title" ! href "" $ "hackman"
            H.br
            H.span "between the abstractions we want and the abstractions we get." ! class_ "subtitle" 
        body