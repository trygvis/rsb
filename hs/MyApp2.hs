import Debug.Trace
import Network.Rest.Cli.Rsb
import Text.XML.HaXml.Parse hiding (content)
import Text.XML.HaXml.Types
import Text.XML.HaXml.Xtract.Parse
import Text.XML.HaXml.Pretty

-----------------

data AtomDocument = AtomDocument {
      documentTitle :: String
    , documentEntries :: [AtomEntry]
}

data AtomEntry = AtomEntry {
      entryTitle :: String
    , entryContent :: String
}

-- type CFilter i = Content i -> [Content i]
-- xtract :: (String -> String) -> String -> CFilter i
-- content :: Content i -> Doc

xmlToAtom :: Document a -> AtomDocument
xmlToAtom (Document _ _ root _) = AtomDocument title entries
    where
        filter = xtract id "/title"
        title = show (content (filter root))
        entries = []

-----------------

stringToAtom :: String -> AtomDocument
stringToAtom text =
    xmlToAtom document
    where
        document = xmlParse "http resource" text

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
