{- |
Module      :  Redland.MidLevel
Maintainer  :  defanor <defanor@uberspace.net>
Stability   :  unstable
Portability :  non-portable (GHC extensions are used)

Mid-level <http://librdf.org/docs/api/index.html Redland RDF library>
bindings: using Haskell naming conventions, common Haskell types,
'ForeignPtr' with finalizers, exceptions, copied structures (as
opposed to the shared ones, as nodes in statement-related functions in
the C API). Closely follows the original API otherwise.

-}

module Redland.MidLevel where

import Foreign
import Foreign.C
import Control.Exception
import Control.Monad

import Redland.LowLevel


-- * Exceptions

data RedlandException = InitializationException
                      -- ^ Happens when an initializer returns NULL.
                      -- Gets thrown by 'initialize'.
                      | OperationException Int
                      -- ^ Happens on non-zero return value where a
                      -- zero is expected. Gets thrown by 'perform'.
                      | StringOperationException
                      -- ^ Gets thrown by 'justCString' and
                      -- 'justSharedCString'.
                      | ParseFailure
    deriving Show

instance Exception RedlandException


-- * FFI utility functions

-- todo: move these into a separate module?

type Initializer a = IO (ForeignPtr a)

-- | Initializes a Redland object, throws 'InitializationException' on
-- failure (i.e., if NULL is returned).
initialize :: IO (Ptr a) -> FinalizerPtr a -> Initializer a
initialize i f = do
  p <- i
  if p == nullPtr
    then throw InitializationException
    else newForeignPtr f p

-- | Performs an operation, throws 'OperationException' on failure
-- (i.e., on non-zero return value).
perform :: IO CInt -> IO ()
perform a = do
  r <- fromIntegral <$> a
  if r == 0
    then pure ()
    else throw (OperationException r)

-- | Initializes a Redland object, performs an action over it, makes
-- sure to call 'finalizeForeignPtr' afterwards.
withNew :: Initializer a -> (ForeignPtr a -> IO b) -> IO b
withNew i = bracket i finalizeForeignPtr

-- | An abstraction to use with 'withCString' and 'withForeignPtr'.
withNullablePtr :: (a -> (Ptr b -> c) -> c) -> Maybe a -> (Ptr b -> c) -> c
withNullablePtr _ Nothing a = a nullPtr
withNullablePtr f (Just x) a = f x a

-- | Checks whether a 'CString' is NULL, frees if it not, returns a
-- Haskell 'String'.
maybeCString :: CString -> IO (Maybe String)
maybeCString cStr = do
  r <- maybeSharedCString cStr
  free cStr
  pure r

maybeSharedCString :: CString -> IO (Maybe String)
maybeSharedCString cStr
  | cStr == nullPtr = pure Nothing
  | otherwise = Just <$> peekCString cStr

-- | Like 'maybeCString', but requires a string to be there, and
-- throws 'StringOperationException' if it isn't.
justCString :: CString -> IO String
justCString cStr = do
  r <- justSharedCString cStr
  free cStr
  pure r

-- | Like 'justCString', but doesn't 'free' the C string.
justSharedCString :: CString -> IO String
justSharedCString cStr
  | cStr == nullPtr = throw StringOperationException
  | otherwise = peekCString cStr


-- * World

-- not calling librdf_world_open
redlandWorld :: Initializer RedlandWorld
redlandWorld = initialize librdf_new_world p_librdf_free_world


-- * Hashes

redlandHash :: ForeignPtr RedlandWorld
            -- ^ world
            -> String
            -- ^ hash factory name (e.g., "memory")
            -> Initializer RedlandHash
redlandHash world name =
  withForeignPtr world $ \world' ->
  withCString name $ \name' ->
  initialize (librdf_new_hash world' name') p_librdf_free_hash

hashPutStrings :: ForeignPtr RedlandHash
               -- ^ hash
               -> String
               -- ^ key
               -> String
               -- ^ value
               -> IO ()
hashPutStrings hash key value =
  withForeignPtr hash $ \hash' ->
  withCString key $ \key' ->
  withCString value $ \value' ->
  perform $ librdf_hash_put_strings hash' key' value'

hashGet :: ForeignPtr RedlandHash
        -- ^ hash
        -> String
        -- ^ key
        -> IO (Maybe String)
hashGet hash key =
  withForeignPtr hash $ \hash' ->
  withCString key $ librdf_hash_get hash' >=> maybeCString

hashGetDel :: ForeignPtr RedlandHash
           -- ^ hash
           -> String
           -- ^ key
           -> IO (Maybe String)
hashGetDel hash key =
  withForeignPtr hash $ \hash' ->
  withCString key $ librdf_hash_get_del hash' >=> maybeCString


-- * RDF Graph (librdf_model)

redlandModel :: ForeignPtr RedlandWorld
             -- ^ world
             -> ForeignPtr RedlandStorage
             -- ^ storage
             -> String
             -- ^ options
             -> Initializer RedlandModel
             -- ^ model
redlandModel world storage opt =
  withForeignPtr world $ \world' ->
  withForeignPtr storage $ \storage' ->
  withCString opt $ \opt' ->
  initialize (librdf_new_model world' storage' opt') p_librdf_free_model

-- | Acts as a 'RedlandQueryResults' initializer.
modelQueryExecute :: ForeignPtr RedlandModel
                  -> ForeignPtr RedlandQuery
                  -> Initializer RedlandQueryResults
modelQueryExecute model query =
  withForeignPtr model $ \model' ->
  withForeignPtr query $ \query' ->
  initialize (librdf_model_query_execute model' query')
  p_librdf_free_query_results

-- | Acts as a 'RedlandStream' initializer.
modelFindStatements :: ForeignPtr RedlandModel
                    -> ForeignPtr RedlandStatement
                    -> Initializer RedlandStream
modelFindStatements model statement =
  withForeignPtr model $ \model' ->
  withForeignPtr statement $ \statement' ->
  initialize (librdf_model_find_statements model' statement')
  p_librdf_free_stream

modelSync :: ForeignPtr RedlandModel -> IO ()
modelSync model =
  withForeignPtr model $ \model' ->
  perform $ librdf_model_sync model'

modelLoad :: ForeignPtr RedlandModel
          -> ForeignPtr RedlandURI
          -- ^ source URI
          -> Maybe String
          -- ^ parser name
          -> Maybe String
          -- ^ MIME type
          -> Maybe (ForeignPtr RedlandURI)
          -- ^ type URI
          -> IO ()
modelLoad model source parser mime tURI =
  withForeignPtr model $ \model' ->
  withForeignPtr source $ \source' ->
  withNullablePtr withCString parser $ \parser' ->
  withNullablePtr withCString mime $ \mime' ->
  withNullablePtr withForeignPtr tURI $ \tURI' ->
  perform $ librdf_model_load model' source' parser' mime' tURI'

-- * RDF term (librdf_node)

redlandNode :: ForeignPtr RedlandWorld -> Initializer RedlandNode
redlandNode world =
  withForeignPtr world $ \world' ->
  initialize (librdf_new_node world') p_librdf_free_node

nodeFromNode :: ForeignPtr RedlandNode -> Initializer RedlandNode
nodeFromNode node =
  withForeignPtr node $ \node' ->
  initialize (librdf_new_node_from_node node') p_librdf_free_node

nodeFromBlankIdentifier :: ForeignPtr RedlandWorld
                        -> Maybe String
                        -> Initializer RedlandNode
nodeFromBlankIdentifier world identifier =
  withForeignPtr world $ \world' ->
  withNullablePtr withCString identifier $ \identifier' ->
  initialize (librdf_new_node_from_blank_identifier world' identifier')
  p_librdf_free_node

nodeFromLiteral :: ForeignPtr RedlandWorld
                -> String
                -> Maybe String
                -> Bool
                -> Initializer RedlandNode
nodeFromLiteral world val xmlLang isXML =
  withForeignPtr world $ \world' ->
  withCString val $ \val' ->
  withNullablePtr withCString xmlLang $ \xmlLang' ->
  let isXML' = if isXML then 1 else 0 in
    initialize (librdf_new_node_from_literal world' val' xmlLang' isXML')
    p_librdf_free_node

nodeFromTypedLiteral :: ForeignPtr RedlandWorld
                     -> String
                     -> Maybe String
                     -> Maybe (ForeignPtr RedlandURI)
                     -> Initializer RedlandNode
nodeFromTypedLiteral world val xmlLang uri =
  withForeignPtr world $ \world' ->
  withCString val $ \val' ->
  withNullablePtr withCString xmlLang $ \xmlLang' ->
  withNullablePtr withForeignPtr uri $ \uri' ->
  initialize (librdf_new_node_from_typed_literal world' val' xmlLang' uri')
  p_librdf_free_node

nodeFromURI :: ForeignPtr RedlandWorld
            -> ForeignPtr RedlandURI
            -> Initializer RedlandNode
nodeFromURI world uri =
  withForeignPtr world $ \world' ->
  withForeignPtr uri $ \uri' ->
  initialize (librdf_new_node_from_uri world' uri')
  p_librdf_free_node

nodeFromURIString :: ForeignPtr RedlandWorld
                  -> String
                  -> Initializer RedlandNode
nodeFromURIString world uriStr =
  withForeignPtr world $ \world' ->
  withCString uriStr $ \uriStr' ->
  initialize (librdf_new_node_from_uri_string world' uriStr')
  p_librdf_free_node

nodeGetBlankIdentifier :: ForeignPtr RedlandNode -> IO String
nodeGetBlankIdentifier node =
  withForeignPtr node $ librdf_node_get_blank_identifier >=> justSharedCString

nodeGetLiteralValue :: ForeignPtr RedlandNode -> IO String
nodeGetLiteralValue node =
  withForeignPtr node $ librdf_node_get_literal_value >=> justSharedCString

nodeGetLiteralValueLanguage :: ForeignPtr RedlandNode -> IO (Maybe String)
nodeGetLiteralValueLanguage node =
  withForeignPtr node $
  librdf_node_get_literal_value_language >=> maybeSharedCString

nodeGetLiteralValueDatatypeURI :: ForeignPtr RedlandNode
                               -> IO (Maybe (ForeignPtr RedlandURI))
nodeGetLiteralValueDatatypeURI node =
  withForeignPtr node $ \node' -> do
  oldURI <- librdf_node_get_literal_value_datatype_uri node'
  if oldURI == nullPtr
    then pure Nothing
    else Just <$> initialize (librdf_new_uri_from_uri oldURI) p_librdf_free_uri

nodeGetLiteralValueIsWellFormedXML :: ForeignPtr RedlandNode -> IO Bool
nodeGetLiteralValueIsWellFormedXML node =
  withForeignPtr node $ fmap (/= 0) . librdf_node_get_literal_value_is_wf_xml

nodeGetURI :: ForeignPtr RedlandNode -> Initializer RedlandURI
nodeGetURI node =
  withForeignPtr node $ \node' -> do
  oldURI <- librdf_node_get_uri node'
  initialize (librdf_new_uri_from_uri oldURI) p_librdf_free_uri

nodeIsBlank :: ForeignPtr RedlandNode -> IO Bool
nodeIsBlank node =
  withForeignPtr node $ fmap (/= 0) . librdf_node_is_blank

nodeIsLiteral :: ForeignPtr RedlandNode -> IO Bool
nodeIsLiteral node =
  withForeignPtr node $ fmap (/= 0) . librdf_node_is_literal

nodeIsResource :: ForeignPtr RedlandNode -> IO Bool
nodeIsResource node =
  withForeignPtr node $ fmap (/= 0) . librdf_node_is_resource

nodeToString :: ForeignPtr RedlandNode -> IO String
nodeToString node =
  withForeignPtr node $ librdf_node_to_string >=> justCString


-- * Parsers

redlandParser :: ForeignPtr RedlandWorld
              -- ^ world
              -> Maybe String
              -- ^ parser name (e.g., "rdfxml", "turtle")
              -> Maybe String
              -- ^ MIME type
              -> Maybe (ForeignPtr RedlandURI)
              -- ^ type URI
              -> Initializer RedlandParser
redlandParser world name mimeType uri =
  withForeignPtr world $ \world' ->
  withNullablePtr withCString name $ \name' ->
  withNullablePtr withCString mimeType $ \mimeType' ->
  withNullablePtr withForeignPtr uri $ \uri' ->
  initialize (librdf_new_parser world' name' mimeType' uri')
  p_librdf_free_parser

parseStringIntoModel :: ForeignPtr RedlandParser
                     -- ^ parser
                     -> String
                     -- ^ string to parse
                     -> ForeignPtr RedlandURI
                     -- ^ base URI
                     -> ForeignPtr RedlandModel
                     -- ^ model
                     -> IO ()
parseStringIntoModel parser str uri model =
  withForeignPtr parser $ \parser' ->
  withCString str $ \str' ->
  withForeignPtr uri $ \uri' ->
  withForeignPtr model $ \model' ->
  perform $ librdf_parser_parse_string_into_model parser' str' uri' model'

parserGuessName2 :: ForeignPtr RedlandWorld
                 -> Maybe String
                 -- ^ MIME type
                 -> Maybe String
                 -- ^ content
                 -> Maybe String
                 -- ^ content identifier
                 -> IO (Maybe String)
parserGuessName2 world mime content contentId =
  withForeignPtr world $ \world' ->
  withNullablePtr withCString mime $ \mime' ->
  withNullablePtr withCString content $ \content' ->
  withNullablePtr withCString contentId $
  librdf_parser_guess_name2 world' mime' content' >=> maybeSharedCString


-- * Querying

redlandQuery :: ForeignPtr RedlandWorld
             -> String
             -- ^ language name
             -> Maybe (ForeignPtr RedlandURI)
             -- ^ language URI
             -> String
             -- ^ query string
             -> Maybe (ForeignPtr RedlandURI)
             -- ^ base URI
             -> Initializer RedlandQuery
redlandQuery world lang langURI query baseURI =
  withForeignPtr world $ \world' ->
  withCString lang $ \lang' ->
  withNullablePtr withForeignPtr langURI $ \langURI' ->
  withCString query $ \query' ->
  withNullablePtr withForeignPtr baseURI $ \baseURI' ->
  initialize (librdf_new_query world' lang' langURI' query' baseURI')
  p_librdf_free_query


-- * Query results

queryResultsGetCount :: ForeignPtr RedlandQueryResults -> IO Int
queryResultsGetCount results =
  withForeignPtr results $ fmap fromIntegral . librdf_query_results_get_count

queryResultsNext :: ForeignPtr RedlandQueryResults
                 -> IO Bool
                 -- ^ 'True' if it's not finished.
queryResultsNext results =
  withForeignPtr results $ fmap (== 0) . librdf_query_results_next

queryResultsFinished :: ForeignPtr RedlandQueryResults -> IO Bool
queryResultsFinished results =
  withForeignPtr results $ fmap (/= 0) . librdf_query_results_finished

-- | Acts as a 'RedlandNode' initializer.
queryResultsGetBindingValue :: ForeignPtr RedlandQueryResults
                            -> Int
                            -> Initializer RedlandNode
queryResultsGetBindingValue results offset =
  withForeignPtr results $ \results' ->
  initialize
  (librdf_query_results_get_binding_value results' (fromIntegral offset))
  p_librdf_free_node

queryResultsGetBindingName :: ForeignPtr RedlandQueryResults
                           -> Int
                           -> IO String
queryResultsGetBindingName results offset =
  withForeignPtr results $ \results' ->
  librdf_query_results_get_binding_name results' (fromIntegral offset)
  >>= justSharedCString

-- | Acts as a 'RedlandNode' initializer.
queryResultsGetBindingValueByName :: ForeignPtr RedlandQueryResults
                                  -> String
                                  -> Initializer RedlandNode
queryResultsGetBindingValueByName results name =
  withForeignPtr results $ \results' ->
  withCString name $ \name' ->
  initialize
  (librdf_query_results_get_binding_value_by_name results' name')
  p_librdf_free_node

queryResultsGetBindingsCount :: ForeignPtr RedlandQueryResults
                             -> IO Int
queryResultsGetBindingsCount results =
  withForeignPtr results $
  fmap fromIntegral . librdf_query_results_get_bindings_count

-- | Acts as a 'RedlandQueryResults' initializer.
queryExecute :: ForeignPtr RedlandQuery
             -> ForeignPtr RedlandModel
             -> Initializer RedlandQueryResults
queryExecute query model =
  withForeignPtr query $ \query' ->
  withForeignPtr model $ \model' ->
  initialize (librdf_query_execute query' model') p_librdf_free_query_results


-- * RDF Triple (librdf_statement)

redlandStatement :: ForeignPtr RedlandWorld -> Initializer RedlandStatement
redlandStatement world =
  withForeignPtr world $ \world' ->
  initialize (librdf_new_statement world') p_librdf_free_statement

statementFromNodes :: ForeignPtr RedlandWorld
                   -> Maybe (ForeignPtr RedlandNode)
                   -- ^ subject
                   -> Maybe (ForeignPtr RedlandNode)
                   -- ^ predicate
                   -> Maybe (ForeignPtr RedlandNode)
                   -- ^ object
                   -> Initializer RedlandStatement
statementFromNodes world subject predicate object =
  withForeignPtr world $ \world' ->
  withNullablePtr withForeignPtr subject $ \subject' ->
  withNullablePtr withForeignPtr predicate $ \predicate' ->
  withNullablePtr withForeignPtr object $ \object' -> do
  sCopy <- librdf_new_node_from_node subject'
  pCopy <- librdf_new_node_from_node predicate'
  oCopy <- librdf_new_node_from_node object'
  initialize
    (librdf_new_statement_from_nodes world' sCopy pCopy oCopy)
    p_librdf_free_statement

-- | An abstraction used for getting statement components.
statementGet :: (Ptr RedlandStatement -> IO (Ptr RedlandNode))
             -> ForeignPtr RedlandStatement
             -> IO (Maybe (ForeignPtr RedlandNode))
statementGet f statement =
  withForeignPtr statement $ \statement' -> do
  oldNode <- f statement'
  if oldNode == nullPtr
    then pure Nothing
    else Just <$>
         initialize (librdf_new_node_from_node oldNode) p_librdf_free_node

-- | An abstraction used for setting statement components.
statementSet :: (Ptr RedlandStatement -> Ptr RedlandNode -> IO ())
             -> ForeignPtr RedlandStatement
             -> Maybe (ForeignPtr RedlandNode)
             -> IO ()
statementSet f statement node =
  withForeignPtr statement $ \statement' ->
  case node of
    Nothing -> f statement' nullPtr
    Just node' -> withForeignPtr node' $ \node'' -> do
      nodeCopy <- librdf_new_node_from_node node''
      f statement' nodeCopy

statementGetSubject :: ForeignPtr RedlandStatement
                    -> IO (Maybe (ForeignPtr RedlandNode))
statementGetSubject = statementGet librdf_statement_get_subject

statementSetSubject :: ForeignPtr RedlandStatement
                    -> Maybe (ForeignPtr RedlandNode)
                    -> IO ()
statementSetSubject = statementSet librdf_statement_set_subject

statementGetPredicate :: ForeignPtr RedlandStatement
                      -> IO (Maybe (ForeignPtr RedlandNode))
statementGetPredicate = statementGet librdf_statement_get_predicate

statementSetPredicate :: ForeignPtr RedlandStatement
                      -> Maybe (ForeignPtr RedlandNode)
                      -> IO ()
statementSetPredicate = statementSet librdf_statement_set_predicate

statementGetObject :: ForeignPtr RedlandStatement
                   -> IO (Maybe (ForeignPtr RedlandNode))
statementGetObject = statementGet librdf_statement_get_object

statementSetObject :: ForeignPtr RedlandStatement
                   -> Maybe (ForeignPtr RedlandNode)
                   -> IO ()
statementSetObject = statementSet librdf_statement_set_object


-- * Triple stores

redlandStorage :: ForeignPtr RedlandWorld
               -- ^ world
               -> String
               -- ^ storage type name ("hashes", "memory", "file",
               -- etc)
               -> String
               -- ^ storage identifier
               -> String
               -- ^ options
               -> Initializer RedlandStorage
redlandStorage world sname name opt =
  withForeignPtr world $ \world' ->
  withCString sname $ \sname' ->
  withCString name $ \name' ->
  withCString opt $ \opt' ->
  initialize (librdf_new_storage world' sname' name' opt')
  p_librdf_free_storage

redlandStorageWithOptions :: ForeignPtr RedlandWorld
                          -- ^ world
                          -> String
                          -- ^ storage type name ("hashes", "memory",
                          -- "file", etc)
                          -> String
                          -- ^ storage identifier
                          -> ForeignPtr RedlandHash
                          -- ^ options
                          -> Initializer RedlandStorage
redlandStorageWithOptions world sname name opt =
  withForeignPtr world $ \world' ->
  withCString sname $ \sname' ->
  withCString name $ \name' ->
  withForeignPtr opt $ \opt' ->
  initialize (librdf_new_storage_with_options world' sname' name' opt')
  p_librdf_free_storage


-- * Stream of triples (librdf_statement)

streamEnd :: ForeignPtr RedlandStream -> IO Bool
streamEnd stream =
  withForeignPtr stream $ fmap (/= 0) . librdf_stream_end

streamNext :: ForeignPtr RedlandStream
           -> IO Bool
           -- ^ 'True' if it's not finished.
streamNext stream =
  withForeignPtr stream $ fmap (== 0) . librdf_stream_next

streamGetObject :: ForeignPtr RedlandStream
                -> Initializer RedlandStatement
streamGetObject stream =
  withForeignPtr stream $ \stream' -> do
  statement <- librdf_stream_get_object stream'
  initialize (librdf_new_statement_from_statement statement)
    p_librdf_free_statement


-- * URI

redlandURI :: ForeignPtr RedlandWorld
           -- ^ world
           -> String
           -- ^ URI string
           -> Initializer RedlandURI
redlandURI world uriStr =
  withForeignPtr world $ \world' ->
  withCString uriStr $ \uriStr' ->
  initialize (librdf_new_uri world' uriStr') p_librdf_free_uri

uriFromURI :: ForeignPtr RedlandURI -> Initializer RedlandURI
uriFromURI uri =
  withForeignPtr uri $ \uri' ->
  initialize (librdf_new_uri_from_uri uri') p_librdf_free_uri

uriAsString :: ForeignPtr RedlandURI -> IO String
uriAsString uri =
  withForeignPtr uri $ librdf_uri_as_string >=> justSharedCString
