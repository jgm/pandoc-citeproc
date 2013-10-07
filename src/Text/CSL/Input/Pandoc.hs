module Text.CSL.Input.Pandoc (blocksToString, inlinesToString)
where
import Text.Pandoc
import Text.TeXMath (texMathToPandoc, DisplayType(..))
import Control.Applicative
import Data.List (intercalate)
import Data.Monoid (mconcat)

blocksToString :: (Functor m, Monad m) => [Block] -> m String
blocksToString = fmap (intercalate "\n\n") . mapM go
  where go (Plain xs) = inlinesToString xs
        go (Para xs)  = inlinesToString xs
        go _          = return ""

inlinesToString :: (Functor m, Monad m) => [Inline] -> m String
inlinesToString = fmap mconcat . mapM go
  where go (Str xs)         = return xs
        go Space            = return " "
        go (Emph xs)        = inTag "i" <$> inlinesToString xs
        go (Strong xs)      = inTag "b" <$> inlinesToString xs
        go (Superscript xs) = inTag "sup" <$> inlinesToString xs
        go (Subscript xs)   = inTag "sub" <$> inlinesToString xs
        go (SmallCaps xs)   = inTag "sc" <$> inlinesToString xs
        go (Code _ xs)      = return xs
        go (Link xs _)      = inlinesToString xs
        go (Image xs _)     = inlinesToString xs
        go (RawInline f xs) | f == Format "citeproc"
                            = return xs
        go (Span _ xs)      = inlinesToString xs
        go (Note _)         = return ""
        go (LineBreak)      = return " "
        go (Math _ xs)      = either (\_ -> return $ surround '$' '$' xs)
                              inlinesToString
                              $ texMathToPandoc DisplayInline xs
        go (Cite _ ils)     = inlinesToString ils
        go (Quoted SingleQuote xs) = surround '‘' '’' <$> inlinesToString xs
        go (Quoted DoubleQuote xs) = surround '“' '”' <$> inlinesToString xs
        go _                = return ""

surround :: Char -> Char -> String -> String
surround beg end s = beg : s ++ [end]

inTag :: String -> String -> String
inTag t s = "<" ++ t ++ ">" ++ s ++ "</" ++ takeWhile (/=' ') t ++ ">"
