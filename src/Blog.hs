{-# LANGUAGE OverloadedStrings #-}

module Blog where

import Text.Markdown
import Text.Blaze.Html.Renderer.Text
import Text.Blaze.Html

type FileName = String

readArticle :: FileName -> Html
readArticle fn = markdown def "# Hello World!"
