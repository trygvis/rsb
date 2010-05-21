{-# LANGUAGE Arrows, NoMonomorphismRestriction #-}
import Debug.Trace
import Network.Rest.Cli.Rsb
-- Text.XML.HXT.Arrow.xread :: ArrowXml a => a String XmlTree
import Text.XML.HXT.Arrow hiding (parseXmlDocument)
-- Text.XML.HXT.Parser.XmlParsec.xread :: String -> XmlTrees
--import Text.XML.HXT.Parser.XmlParsec(xread)
--import Text.XML.HXT.Parser.XmlParsec(parseXmlDocument)
--import Text.XML.HXT.DOM.XmlTreeFilter
--import Text.XML.HXT.DOM.XmlTree(isXText)
--import Text.XML.HXT.DOM.ShowXml

-----------------

data AtomDocument = AtomDocument {
      documentTitle :: String
    , documentEntries :: [AtomEntry]
}

data AtomEntry = AtomEntry {
      entryTitle :: String
    , entryContent :: String
}

xmlToAtom :: ArrowXml a => a XmlTree AtomDocument
xmlToAtom = atTag "root" >>> proc x -> do
    title <- getFeedTitle -< x
    returnA -< AtomDocument title []

--getFeed :: ArrowXml a => XmlTree AtomDocument
--getFeed = atTag "feed" >>> listA getElem

getFeedTitle :: ArrowXml a => a XmlTree String
getFeedTitle = getChildren >>> hasName "title" >>> getText

getEntries :: ArrowXml a => a XmlTree [AtomEntry]
getEntries = error("getEntries")

atTag :: ArrowXml a => String -> a XmlTree XmlTree
atTag tag = deep $ isElem >>> hasName tag

-----------------

stringToAtom :: String -> AtomDocument
stringToAtom text =
    head $ runLA (xread >>> xmlToAtom) text

myApp2Function :: String -> Response String
myApp2Function s = rsbReturn s

myApp2 :: Uri -> Response String
myApp2 uri = subRequest "http://github.com/javaBin/ems/commits/master.atom" (RsbFunction "myApp2Function" myApp2Function)

main :: IO ()
main = 
    let
        req = "http://host/path"
    in
        do
            runApp req myApp2
