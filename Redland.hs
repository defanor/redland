{- |
Module      :  Redland
Maintainer  :  defanor <defanor@uberspace.net>
Stability   :  unstable
Portability :  non-portable (GHC extensions are used)

<http://librdf.org/ Redland RDF library> bindings. See
<http://librdf.org/docs/api/index.html the original API> for in-depth
descriptions.

= Library organization

- Raw bindings are provided by "Redland.LowLevel". Normally they
  should not be used directly.

- Refined versions (using Haskell types) of those are provided by
  "Redland.MidLevel". One should still be careful with the allocated
  resources while using those, for instance by using 'withNew'. A rule
  of thumb is that whenever you see an 'Initializer', it's a good idea
  to wrap it into 'withNew'.

- Utility functions and types are provided by "Redland.Util". Those
  don't strictly correspond to functions of the original API.


= Usage example

> import Redland
>
> input :: String
> input = "<?xml version=\"1.0\"?>\
> \<rdf:RDF xmlns:rdf=\"http://www.w3.org/1999/02/22-rdf-syntax-ns#\"\
> \     xmlns:dc=\"http://purl.org/dc/elements/1.1/\">\
> \  <rdf:Description rdf:about=\"http://www.dajobe.org/\">\
> \    <dc:title>Dave Beckett's Home Page</dc:title>\
> \    <dc:creator>Dave Beckett</dc:creator>\
> \    <dc:description>The generic home page of Dave Beckett.</dc:description>\
> \  </rdf:Description>\
> \</rdf:RDF>\
> \"
>
> main :: IO ()
> main =
>   withWSMU "memory" [] "example" "" "http://example.librdf.org/" $
>   \world storage model uri -> do
>     -- parse and insert
>     guessingParseStringIntoModel world model uri input
>     -- query
>     withQuery world model "sparql"
>       "SELECT ?foo ?bar ?baz WHERE { ?foo ?bar ?baz }" (Just uri) $
>       mapM_ print
>     -- search statements
>     withStatements world model (Triple Nothing Nothing Nothing) $
>       mapM_ print

It prints the following:

> [("foo",ResourceNode "http://www.dajobe.org/"),("bar",ResourceNode "http://purl.org/dc/elements/1.1/title"),("baz",LiteralNode "Dave Beckett's Home Page" Nothing)]
> [("foo",ResourceNode "http://www.dajobe.org/"),("bar",ResourceNode "http://purl.org/dc/elements/1.1/creator"),("baz",LiteralNode "Dave Beckett" Nothing)]
> [("foo",ResourceNode "http://www.dajobe.org/"),("bar",ResourceNode "http://purl.org/dc/elements/1.1/description"),("baz",LiteralNode "The generic home page of Dave Beckett." Nothing)]
> Triple {subject = Just (ResourceNode "http://www.dajobe.org/"), predicate = Just (ResourceNode "http://purl.org/dc/elements/1.1/title"), object = Just (LiteralNode "Dave Beckett's Home Page" Nothing)}
> Triple {subject = Just (ResourceNode "http://www.dajobe.org/"), predicate = Just (ResourceNode "http://purl.org/dc/elements/1.1/creator"), object = Just (LiteralNode "Dave Beckett" Nothing)}
> Triple {subject = Just (ResourceNode "http://www.dajobe.org/"), predicate = Just (ResourceNode "http://purl.org/dc/elements/1.1/description"), object = Just (LiteralNode "The generic home page of Dave Beckett." Nothing)}

-}

module Redland ( module Redland.MidLevel
               , module Redland.Util
               , RedlandWorld
               , RedlandHash
               , RedlandModel
               , RedlandNode
               , RedlandParser
               , RedlandQuery
               , RedlandQueryResults
               , RedlandStatement
               , RedlandStorage
               , RedlandStream
               , RedlandURI
               , ForeignPtr
               )
where

import Foreign
import Redland.LowLevel
import Redland.MidLevel
import Redland.Util
