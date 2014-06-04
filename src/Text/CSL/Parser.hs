{-# LANGUAGE PatternGuards #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}
-----------------------------------------------------------------------------
-- |
-- Module      :  Text.CSL.Parser
-- Copyright   :  (c) Andrea Rossato
-- License     :  BSD-style (see LICENSE)
--
-- Maintainer  :  Andrea Rossato <andrea.rossato@unitn.it>
-- Stability   :  unstable
-- Portability :  unportable
--
-- Parser for Style
--
-----------------------------------------------------------------------------

module Text.CSL.Parser ( readCSLFile, parseCSL, parseCSL', localizeCSL )
where

import Text.CSL.Style
import Text.Pandoc.UTF8 (fromStringLazy)
import qualified Data.Map as M
import Text.CSL.Util ( readNum, toRead, toShow, readable )

import Text.CSL.Pickle
import Text.CSL.Data    ( getLocale )
import Data.Maybe       ( catMaybes, fromMaybe )
import qualified Data.ByteString.Lazy as L
import System.Directory (getAppUserDataDirectory)
import Text.CSL.Util (findFile)
#ifdef USE_NETWORK
import Network.HTTP ( getResponseBody, mkRequest, RequestMethod(..) )
import Network.Browser ( browse, setAllowRedirects, setUserAgent, setOutHandler, request )
import Network.URI ( parseURI, URI(..) )
#endif

-- | Read and parse a CSL style file into a localized sytle.
readCSLFile :: Maybe String -> FilePath -> IO Style
readCSLFile mbLocale src = do
  csldir <- getAppUserDataDirectory "csl"
#ifdef USE_NETWORK
  let readURI u = do rsp <- browse $ do
                              setAllowRedirects True
                              setOutHandler (const (return ()))
                              setUserAgent "citeproc-hs"
                              request $ mkRequest GET u
                     getResponseBody (Right $ snd rsp)
  f <- case parseURI src of
         Just u | uriScheme u `elem` ["http:","https:"] -> readURI u
         _      -> findFile [".", csldir] src >>= L.readFile
#else
  f <- findFile [".", csldir] src >>= L.readFile
#endif
  localizeCSL mbLocale $ parseCSL' f

-- | Parse a 'String' into a 'Style' (with default locale).
parseCSL :: String -> Style
parseCSL = parseCSL' . fromStringLazy

parseCSL' :: L.ByteString -> Style
parseCSL' f = readXmlString xpStyle f

-- | Merge locale into a CSL style.
localizeCSL :: Maybe String -> Style -> IO Style
localizeCSL mbLocale s = do
  let locale = fromMaybe (styleDefaultLocale s) mbLocale
  l <- readXmlString xpLocale `fmap` getLocale locale
  return s { styleLocale = mergeLocales locale l (styleLocale s) }

instance XmlPickler Layout where
    xpickle = xpWrap (uncurry3 Layout, \(Layout f d e) -> (f,d,e)) $
              xpIElem "layout" $
              xpTriple xpickle xpDelimiter xpickle

instance XmlPickler Element where
    xpickle = xpAlt tag ps
        where
          tag (Choose       {}) =  0
          tag (Macro        {}) =  1
          tag (Const        {}) =  2
          tag (Variable     {}) =  4
          tag (Term         {}) =  5
          tag (Label        {}) =  6
          tag (Names        {}) =  7
          tag (Substitute   {}) =  9
          tag (Group        {}) = 10
          tag (Number       {}) = 11
          tag (Date         {}) = 12
          tag (Elements     {}) = 13
          ps = [ xpChoose
               , xpMacro
               , xpConst
               , xpVariable
               , xpTerm
               , xpLabel
               , xpNames
               , xpSubStitute
               , xpGroup
               , xpNumber
               , xpDate
               ]

instance XmlPickler IfThen where
    xpickle = xpWrap (uncurry3 IfThen, \(IfThen c m e) -> (c,m,e)) $
              xpTriple xpickle xpickle xpickle

instance XmlPickler Condition where
    xpickle = xpWrap ( \ ((t,v,n),(d,p,a,l)) ->
                           Condition (words t) (words v) (words n)
                                     (words d) (words p) (words a) (words l),
                       \ (Condition t v n d p a l) ->
                           ((unwords t,unwords v,unwords n)
                           ,(unwords d,unwords p,unwords a,unwords l))) $
              xpPair (xpTriple (xpAttrText' "type"             )
                               (xpAttrText' "variable"         )
                               (xpAttrText' "is-numeric"       ))
                     (xp4Tuple (xpAttrText' "is-uncertain-date")
                               (xpAttrText' "position"         )
                               (xpAttrText' "disambiguate"     )
                               (xpAttrText' "locator"          ))

instance XmlPickler Formatting where
    xpickle = xpWrap ( \(((p,s,ff),(fs,fv,fw)),(td,va,tc,d),(q,sp))
                         -> Formatting p s ff fs fv fw td va tc d
                            (if q then NativeQuote else NoQuote) sp False False
                     , \(Formatting p s ff fs fv fw td va tc d _ sp _ _)
                         -> (((p,s,ff),(fs,fv,fw)),(td,va,tc,d),(False,sp))) $
              xpTriple (xpPair (xpTriple (xpAttrText' "prefix"      )
                                         (xpAttrText' "suffix"      )
                                         (xpAttrText' "font-family" ))
                               (xpTriple (xpAttrText' "font-style"  )
                                         (xpAttrText' "font-variant")
                                         (xpAttrText' "font-weight" )))
                       (xp4Tuple (xpAttrText' "text-decoration")
                                 (xpAttrText' "vertical-align" )
                                 (xpAttrText' "text-case"      )
                                 (xpAttrText' "display"        ))
                       (xpPair   (xpAttrWithDefault False "quotes"        xpickle)
                                 (xpAttrWithDefault False "strip-periods" xpickle))

instance XmlPickler Sort where
    xpickle = xpAlt tag ps
        where
          readSort = read . flip (++) " \"\"" . toRead
          tag (SortVariable {}) = 0
          tag (SortMacro    {}) = 1
          ps = [ xpWrap ( \(v,s) -> SortVariable v (readSort s)
                        , \(SortVariable v s) -> (v,toShow $ show s)) $
                 xpElem "key" $
                 xpPair (xpAttrText "variable")
                        (xpAttrWithDefault "ascending" "sort" xpText)

               , xpWrap ( \(v,s,a,b,c) -> SortMacro v (readSort s) (readNum a) (readNum b) c
                        , \(SortMacro v s a b c) -> (v,toShow $ show s,show a,show b, c)) $
                 xpElem "key" $
                 xp5Tuple (xpAttrText "macro")
                          (xpAttrWithDefault "ascending" "sort"            xpText)
                          (xpAttrWithDefault ""          "names-min"       xpText)
                          (xpAttrWithDefault ""          "names-use-first" xpText)
                          (xpAttrWithDefault ""          "names-use-last"  xpText)
               ]

instance XmlPickler Bool where
    xpickle = xpWrap readable xpText

instance XmlPickler Gender where
    xpickle = xpWrap readable xpText

instance XmlPickler Form where
    xpickle = xpWrap readable
                     (xpAttrWithDefault "long" "form" xpText)

instance XmlPickler NumericForm where
    xpickle = xpWrap readable
                     (xpAttrWithDefault "numeric" "form" xpText)

instance XmlPickler DateForm where
    xpickle = xpWrap (read . toRead . flip (++) "-date", const [])
                     (xpAttrWithDefault "no-form" "form" xpText)

instance XmlPickler Match where
    xpickle = xpWrap readable
                     (xpAttrWithDefault "all" "match" xpText)

instance XmlPickler DatePart where
    xpickle = xpWrap (uncurry4 DatePart, \(DatePart s f d fm) -> (s,f,d,fm)) $
              xpElem "date-part" $
              xp4Tuple (xpAttrText "name")
                       (xpAttrWithDefault "long" "form"            xpText)
                       (xpAttrWithDefault "-"    "range-delimiter" xpText)
                        xpickle

instance XmlPickler Name where
    xpickle = xpAlt tag ps
        where
          tag (Name      {}) = 0
          tag (NameLabel {}) = 1
          tag (EtAl      {}) = 2
          ps = [ xpWrap (uncurry5 Name, \(Name f fm nas d nps) -> (f,fm,nas,d,nps)) $
                 xpElem "name"  $ xp5Tuple xpNameForm xpickle xpNameAttrs xpDelimiter xpickle
               , xpWrap (uncurry3 NameLabel, \(NameLabel f fm p) -> (f, fm,p)) $
                 xpElem "label" $ xpTriple xpickle xpickle xpPlural
               , xpWrap (uncurry EtAl, \(EtAl fm t) -> (fm,t)) $
                 xpElem "et-al" $ xpPair xpickle $ xpAttrText' "term"
               ]
          xpNameForm = xpWrap readable $ xpAttrWithDefault "not-set" "form" xpText

instance XmlPickler NamePart where
    xpickle = xpWrap (uncurry NamePart, \(NamePart s fm) -> (s,fm)) $
              xpElem "name-part" $
              xpPair (xpAttrText "name")
                      xpickle

instance XmlPickler CSInfo where
    xpickle = xpWrap ( \ ((t,i,u),(a,c)) -> CSInfo t a c i u
                     , \ s -> ((csiTitle s,  csiId s, csiUpdated s)
                              ,(csiAuthor s, csiCategories s))) $
              xpPair (xpTriple (get "title"  )
                               (get "id"     )
                               (get "updated"))
                     (xpPair   (xpIElemWithDefault (CSAuthor   "" "" "") "author" xpickle)
                               (xpDefault [] $ xpList $ xpIElem "category" xpickle))
                  where
                    get = flip xpIElem xpText

instance XmlPickler CSAuthor where
    xpickle = xpWrap   (uncurry3 CSAuthor, \(CSAuthor a b c) -> (a, b, c)) $
              xpTriple (xpIElemWithDefault [] "name"  xpText)
                       (xpIElemWithDefault [] "email" xpText)
                       (xpIElemWithDefault [] "uri"   xpText)

instance XmlPickler CSCategory where
    xpickle = xpWrap   (uncurry3 CSCategory, \(CSCategory a b c) -> (a, b, c)) $
              xpTriple (xpAttrText  "term"  )
                       (xpAttrText' "schema")
                       (xpAttrText' "label" )

xpStyle :: PU Style
xpStyle
    = xpWrap ( \ ((v,sc,si,sl,l),(o,m,c,b))   -> Style v sc si sl l (Abbreviations M.empty) o m c b
             , \ (Style v sc si sl l _ o m c b) -> ((v,sc,si,sl,l),(o,m,c,b))) $
      xpIElem "style" $
      xpPair (xp5Tuple (xpAttrText "version")
                       (xpAttrText "class")
                        xpInfo
                       (xpAttrWithDefault "en-US" "default-locale" xpText)
                       (xpList xpLocale))
             (xp4Tuple  xpStyleOpts
                        xpMacros
                        xpCitation
                       (xpOption xpBibliography))

xpInfo :: PU (Maybe CSInfo)
xpInfo  = xpOption . xpIElem "info" $ xpickle

xpLocale :: PU Locale
xpLocale
    = xpWrap ( \ ((v,l),(o,t,d))   -> Locale v l o t d
             , \ (Locale v l o t d) -> ((v,l),(o,t,d))) $
      xpIElem "locale" $
      xpPair (xpPair   (xpAttrText' "version" )
                       (xpAttrText' "lang"))
             (xpTriple (xpIElemWithDefault [] "style-options" $  xpOpt "punctuation-in-quote")
                        xpTerms
                        (xpList xpLocaleDate))

xpTerms :: PU [CslTerm]
xpTerms
    = xpWrap (concat,return) $ xpList $
      xpIElem "terms" $ xpList $ xpElem "term" $
      xpWrap (\((n,f,g,gf,m),(s,p)) -> CT n f g gf s p m,
             undefined) $
             xpPair (xp5Tuple (xpAttrText "name")
                              xpickle
                              (xpAttrWithDefault Neuter "gender" xpickle)
                              (xpAttrWithDefault Neuter "gender-form" xpickle)
                              (xpAttrText' "match"))
                    (xpChoice (xpWrap (\s -> (s,s), fst)  xpText0)
                              (xpPair (xpIElem "single"   xpText0)
                              (xpIElem "multiple" xpText0))
                              xpLift)

xpMacros :: PU [MacroMap]
xpMacros
    = xpList $ xpIElem "macro" $
      xpPair (xpAttrText "name") xpickle

xpCitation :: PU Citation
xpCitation
    = xpWrap (uncurry3 Citation, \(Citation o s l) -> (o,s,l)) $
      xpIElem "citation" $
      xpTriple xpCitOpts xpSort xpickle

xpBibliography :: PU Bibliography
xpBibliography
    = xpWrap (uncurry3 Bibliography, \(Bibliography o s l) -> (o,s,l)) $
      xpIElem "bibliography" $
      xpTriple xpBibOpts xpSort xpickle

xpOpt :: String -> PU [Option]
xpOpt n
    = xpWrap (\a -> filter ((/=) [] . snd) $ [(n,a)], const []) $
      xpAttrText' n

xpNamesOpt :: PU [Option]
xpNamesOpt = xpOpt "names-delimiter"

xpNameFormat :: PU [Option]
xpNameFormat
    = xpWrap (\(a,b,c,d,e,f) ->
                  catMaybes [ checkOpt "and"                     a
                            , checkOpt "delimiter-precedes-last" b
                            , checkOpt "sort-separator"          c
                            , checkOpt "initialize"              d
                            , checkOpt "initialize-with"         e
                            , checkOpt "name-as-sort-order"      f
                            ] , const (Nothing, Nothing, Nothing, Nothing, Nothing, Nothing)) $
      xp6Tuple (getOpt "and")
               (getOpt "delimiter-precedes-last")
               (getOpt "sort-separator")
               (getOpt "initialize")
               (getOpt "initialize-with")
               (getOpt "name-as-sort-order")

    where
      getOpt n = xpOption $ xpAttr n xpText
      checkOpt _ Nothing  = Nothing
      checkOpt n (Just s) = Just (n,s)

xpNameAttrs :: PU NameAttrs
xpNameAttrs
    = xpWrap (\((a,b,c,d,e),(f,g)) ->
                  filter ((/=) [] . snd) [("et-al-min",a)
                                         ,("et-al-use-first",b)
                                         ,("et-al-subsequent-min",c)
                                         ,("et-al-subsequent-use-first",d)
                                         ,("et-al-use-last",e)
                                         ,("delimiter-precedes-et-al",f)] ++ g
             , const (([],[],[],[],[]),([],[]))) $
      xpPair (xp5Tuple (xpAttrText' "et-al-min")
                       (xpAttrText' "et-al-use-first")
                       (xpAttrText' "et-al-subsequent-min")
                       (xpAttrText' "et-al-subsequent-use-first")
                       (xpAttrText' "et-al-use-last")) $
              xpPair   (xpAttrText' "delimiter-precedes-et-al")
                       xpNameFormat

xpNameOpt :: PU [Option]
xpNameOpt
    = xpWrap (\(a,b,c) ->
                  filter ((/=) [] . snd) $ a ++ [("name-delimiter",b)
                                                ,("name-form",c)], const ([],[],[])) $
      xpTriple  xpNameAttrs
               (xpAttrText' "name-delimiter")
               (xpAttrText' "name-form")

xpBibOpts :: PU [Option]
xpBibOpts
    = xpWrap ( \((a,b,c,d,e,f),(g,h)) ->
                 filter ((/=) [] . snd) $ [("hanging-indent",a)
                                          ,("second-field-align",b)
                                          ,("subsequent-author-substitute",c)
                                          ,("subsequent-author-substitute-rule",d)
                                          ,("line-spacing",e)
                                          ,("entry-spacing",f)] ++ g ++ h
                , const (([],[],[],[],[],[]),([],[]))) $
      xpPair (xp6Tuple (xpAttrText' "hanging-indent")
                       (xpAttrText' "second-field-align")
                       (xpAttrText' "subsequent-author-substitute")
                       (xpAttrText' "subsequent-author-substitute-rule")
                       (xpAttrText' "line-spacing")
                       (xpAttrText' "entry-spacing")) $
             xpPair xpNameOpt xpNamesOpt

xpCitOpts :: PU [Option]
xpCitOpts
    = xpWrap ( \((a,b,c),(d,e,f,g,h,i),(j,k)) ->
                 filter ((/=) [] . snd) $ [("disambiguate-add-names",a)
                                          ,("disambiguate-add-givenname",b)
                                          ,("disambiguate-add-year-suffix",c)
                                          ,("givenname-disambiguation-rule",d)
                                          ,("collapse",e)
                                          ,("cite-group-delimiter",f)
                                          ,("year-suffix-delimiter",g)
                                          ,("after-collapse-delimiter",h)
                                          ,("near-note-distance",i)] ++ j ++ k
                , const (([],[],[]),([],[],[],[],[],[]),([],[]))) $
      xpTriple (xpTriple (xpAttrText' "disambiguate-add-names")
                         (xpAttrText' "disambiguate-add-givenname")
                         (xpAttrText' "disambiguate-add-year-suffix"))
               (xp6Tuple (xpAttrText' "givenname-disambiguation-rule")
                         (xpAttrText' "collapse")
                         (xpAttrText' "cite-group-delimiter")
                         (xpAttrText' "year-suffix-delimiter")
                         (xpAttrText' "after-collapse-delimiter")
                         (xpAttrText' "near-note-distance"))
               (xpPair xpNameOpt xpNamesOpt)

xpStyleOpts :: PU [Option]
xpStyleOpts
    = xpWrap ( \((a,b,c),(d,e)) ->
                 filter ((/=) [] . snd) $ [("page-range-format",a)
                                          ,("demote-non-dropping-particle",b)
                                          ,("initialize-with-hyphen",c)] ++ d ++ e
                , const (([],[],[]),([],[]))) $
      xpPair (xpTriple (xpAttrText' "page-range-format")
                       (xpAttrText' "demote-non-dropping-particle")
                       (xpAttrText' "initialize-with-hyphen")) $
               (xpPair xpNameOpt xpNamesOpt)

xpSort :: PU [Sort]
xpSort
    = xpDefault [] $ xpElem "sort" $ xpList xpickle

xpChoose :: PU Element
xpChoose
    = xpWrap (uncurry3 Choose, \(Choose b t e) -> (b,t,e)) $
      xpElem "choose" $
      xpTriple (                        xpElem "if"      xpickle)
               (xpDefault [] $ xpList $ xpElem "else-if" xpickle)
               (xpDefault []          $ xpElem "else"    xpickle)

xpMacro :: PU Element
xpMacro
    = xpWrap (uncurry Macro, \(Macro s fm) -> (s,fm)) $
      xpTextElem $ xpPair (xpAttrText "macro") xpickle

xpConst :: PU Element
xpConst
    = xpWrap (uncurry Const, \(Const s fm) -> (s,fm)) $
      xpTextElem $ xpPair (xpAttrText "value") xpickle

xpVariable :: PU Element
xpVariable
    = xpWrap ( \((v,f,fm),d)        -> Variable (words v) f fm d
             , \(Variable v f fm d) -> ((unwords v,f,fm),d)) $
      xpTextElem $ xpPair (xpCommon "variable") xpDelimiter

xpTerm :: PU Element
xpTerm
    = xpWrap ( \((t,f,fm),p)    -> Term t f fm p
             , \(Term t f fm p) -> ((t,f,fm),p)) $
      xpTextElem $ xpPair (xpCommon "term") $
                   xpAttrWithDefault True "plural" xpickle

xpNames :: PU Element
xpNames
    = xpWrap ( \((a,n,fm),d,sb)     -> Names (words a) n fm d sb
             , \(Names a n fm d sb) -> ((unwords a,n,fm),d,sb)) $
      xpElem "names" $ xpTriple names xpDelimiter xpickle
    where names    = xpTriple (xpAttrText "variable") xpName xpickle
          xpName   = xpChoice xpZero xpickle check
          check [] = xpLift [Name NotSet emptyFormatting [] [] []]
          check  l = if any isName l then xpLift l else xpZero

xpLabel :: PU Element
xpLabel
    = xpWrap ( uncurry4 Label
             , \(Label s f fm p) -> (s,f,fm,p)) $
      xpElem "label" $
      xp4Tuple (xpAttrText' "variable")
                xpickle xpickle xpPlural

xpSubStitute :: PU Element
xpSubStitute
    = xpWrap (Substitute, \(Substitute es) -> es) $
      xpElem "substitute" xpickle

xpGroup :: PU Element
xpGroup
    = xpWrap (uncurry3 Group, \(Group fm d e) -> (fm,d,e)) $
      xpElem "group" $
      xpTriple xpickle xpDelimiter xpickle

xpNumber :: PU Element
xpNumber
    = xpWrap (uncurry3 Number, \(Number s f fm) -> (s,f,fm)) $
      xpElem "number" $ xpCommon "variable"

xpDate :: PU Element
xpDate
    = xpWrap ( \((s,f,fm),(d,dp,dp'))    -> Date (words s) f fm d dp dp'
             , \(Date s f fm d dp dp') -> ((unwords s,f,fm),(d,dp,dp'))) $
      xpElem  "date" $
      xpPair (xpCommon "variable")
             (xpTriple xpDelimiter xpickle (xpAttrText' "date-parts"))

xpLocaleDate :: PU Element
xpLocaleDate
    = xpWrap ( \((s,f,fm),(d,dp,dp'))    -> Date (words s) f fm d dp dp'
             , \(Date s f fm d dp dp') -> ((unwords s,f,fm),(d,dp,dp'))) $
      xpIElem  "date" $
      xpPair  (xpTriple (xpLift []) xpickle xpickle)
              (xpTriple xpDelimiter xpickle (xpLift []))

xpTextElem :: PU a -> PU a
xpTextElem = xpElem "text"

xpDelimiter :: PU String
xpDelimiter = xpAttrText' "delimiter"

xpPlural :: PU Plural
xpPlural = xpWrap readable $ xpAttrWithDefault "contextual" "plural" xpText

xpCommon :: (XmlPickler b, XmlPickler c) => String -> PU (String,b,c)
xpCommon s = xpTriple (xpAttrText s) xpickle xpickle

-- | For mandatory attributes.
xpAttrText :: String -> PU String
xpAttrText n = xpAttr n xpText

-- | For optional attributes.
xpAttrText' ::  String -> PU String
xpAttrText' n = xpAttrWithDefault [] n xpText

xpAttrWithDefault :: Eq a => a -> String -> PU a -> PU a
xpAttrWithDefault d n = xpDefault d . xpAttr n

xpIElemWithDefault :: Eq a => a -> String -> PU a -> PU a
xpIElemWithDefault d n = xpDefault d . xpIElem n


