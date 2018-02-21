{- |
Module      :  Redland.LowLevel
Maintainer  :  defanor <defanor@uberspace.net>
Stability   :  unstable
Portability :  non-portable (GHC extensions are used)

Low-level <http://librdf.org/docs/api/index.html Redland RDF library>
bindings, a straightforward translation.

-}

{-# LANGUAGE ForeignFunctionInterface #-}

module Redland.LowLevel where

import Foreign
import Foreign.C


-- * World

data RedlandWorld

foreign import ccall "librdf_new_world"
  librdf_new_world :: IO (Ptr RedlandWorld)
foreign import ccall "librdf_free_world"
  librdf_free_world :: Ptr RedlandWorld -> IO ()
foreign import ccall "redland.h &librdf_free_world"
  p_librdf_free_world :: FinalizerPtr RedlandWorld

foreign import ccall "librdf_world_open"
  librdf_world_open :: Ptr RedlandWorld -> IO ()


-- * Hashes

data RedlandHash

foreign import ccall "librdf_new_hash"
  librdf_new_hash
  :: Ptr RedlandWorld
  -- ^ world
  -> CString
  -- ^ name
  -> IO (Ptr RedlandHash)
foreign import ccall "librdf_free_hash"
  librdf_free_hash :: Ptr RedlandHash -> IO ()
foreign import ccall "redland.h &librdf_free_hash"
  p_librdf_free_hash :: FinalizerPtr RedlandHash

foreign import ccall "librdf_hash_put_strings"
  librdf_hash_put_strings
  :: Ptr RedlandHash
  -- ^ hash
  -> CString
  -- ^ key
  -> CString
  -- ^ value
  -> IO CInt
  -- ^ non-zero on failure
foreign import ccall "librdf_hash_get"
  librdf_hash_get
  :: Ptr RedlandHash
  -- ^ hash
  -> CString
  -- ^ key
  -> IO CString
  -- ^ must be freed by the caller
foreign import ccall "librdf_hash_get_del"
  librdf_hash_get_del
  :: Ptr RedlandHash
  -- ^ hash
  -> CString
  -- ^ key
  -> IO CString
  -- ^ must be freed by the caller


-- * RDF Graph (librdf_model)

data RedlandModel

foreign import ccall "librdf_new_model"
  librdf_new_model
  :: Ptr RedlandWorld
  -- ^ world
  -> Ptr RedlandStorage
  -- ^ storage
  -> CString
  -- ^ options
  -> IO (Ptr RedlandModel)
foreign import ccall "librdf_free_model"
  librdf_free_model :: Ptr RedlandModel -> IO ()
foreign import ccall "redland.h &librdf_free_model"
  p_librdf_free_model :: FinalizerPtr RedlandModel

foreign import ccall "librdf_model_query_execute"
  librdf_model_query_execute
  :: Ptr RedlandModel
  -- ^ model
  -> Ptr RedlandQuery
  -- ^ query
  -> IO (Ptr RedlandQueryResults)
  -- ^ NULL on failure
foreign import ccall "librdf_model_sync"
  librdf_model_sync :: Ptr RedlandModel -> IO CInt
foreign import ccall "librdf_model_load"
  librdf_model_load
  :: Ptr RedlandModel
  -> Ptr RedlandURI
  -- ^ source URI
  -> CString
  -- ^ parser name, can be NULL
  -> CString
  -- ^ MIME type, can be NULL
  -> Ptr RedlandURI
  -- ^ type URI, can be NULL
  -> IO CInt

foreign import ccall "librdf_model_find_statements"
  librdf_model_find_statements
  :: Ptr RedlandModel
  -> Ptr RedlandStatement
  -> IO (Ptr RedlandStream)


-- * RDF term (librdf_node)

data RedlandNode

foreign import ccall "librdf_new_node"
  librdf_new_node :: Ptr RedlandWorld -> IO (Ptr RedlandNode)
foreign import ccall "librdf_free_node"
  librdf_free_node :: Ptr RedlandNode -> IO ()
foreign import ccall "redland.h &librdf_free_node"
  p_librdf_free_node :: FinalizerPtr RedlandNode

foreign import ccall "librdf_new_node_from_blank_identifier"
  librdf_new_node_from_blank_identifier
  :: Ptr RedlandWorld
  -> CString
  -- ^ blank node identifier, can be NULL
  -> IO (Ptr RedlandNode)
foreign import ccall "librdf_new_node_from_literal"
  librdf_new_node_from_literal
  :: Ptr RedlandWorld
  -> CString
  -- ^ string value
  -> CString
  -- ^ literal XML language, can be NULL
  -> CInt
  -- ^ non-zero if literal is XML
  -> IO (Ptr RedlandNode)
foreign import ccall "librdf_new_node_from_typed_literal"
  librdf_new_node_from_typed_literal
  :: Ptr RedlandWorld
  -> CString
  -- ^ string value
  -> CString
  -- ^ literal XML language, can be NULL
  -> Ptr RedlandURI
  -- ^ typed literal datatype URI, can be NULL
  -> IO (Ptr RedlandNode)
foreign import ccall "librdf_new_node_from_uri"
  librdf_new_node_from_uri
  :: Ptr RedlandWorld
  -> Ptr RedlandURI
  -> IO (Ptr RedlandNode)
foreign import ccall "librdf_new_node_from_uri_string"
  librdf_new_node_from_uri_string
  :: Ptr RedlandWorld
  -> CString
  -> IO (Ptr RedlandNode)

foreign import ccall "librdf_node_get_blank_identifier"
  librdf_node_get_blank_identifier
  :: Ptr RedlandNode
  -> IO CString
  -- ^ identifier
foreign import ccall "librdf_node_get_literal_value"
  librdf_node_get_literal_value
  :: Ptr RedlandNode
  -> IO CString
  -- ^ Literal value, must be copied
foreign import ccall "librdf_node_get_uri"
  librdf_node_get_uri
  :: Ptr RedlandNode
  -> IO (Ptr RedlandURI)
  -- ^ URI, must be copied
foreign import ccall "librdf_node_is_blank"
  librdf_node_is_blank :: Ptr RedlandNode -> IO CInt
foreign import ccall "librdf_node_is_literal"
  librdf_node_is_literal :: Ptr RedlandNode -> IO CInt
foreign import ccall "librdf_node_is_resource"
  librdf_node_is_resource :: Ptr RedlandNode -> IO CInt
foreign import ccall "librdf_node_to_string"
  librdf_node_to_string :: Ptr RedlandNode -> IO CString


-- * Parsers

data RedlandParser

foreign import ccall "librdf_new_parser"
  librdf_new_parser
  :: Ptr RedlandWorld
  -- ^ world
  -> CString
  -- ^ name
  -> CString
  -- ^ MIME type
  -> Ptr RedlandURI
  -- ^ type URI
  -> IO (Ptr RedlandParser)
foreign import ccall "librdf_free_parser"
  librdf_free_parser :: Ptr RedlandParser -> IO ()
foreign import ccall "redland.h &librdf_free_parser"
  p_librdf_free_parser :: FinalizerPtr RedlandParser

foreign import ccall "librdf_parser_parse_string_into_model"
  librdf_parser_parse_string_into_model
  :: Ptr RedlandParser
  -- ^ parser
  -> CString
  -- ^ string to parse
  -> Ptr RedlandURI
  -- ^ base URI
  -> Ptr RedlandModel
  -- ^ model
  -> IO CInt
  -- ^ non-zero on failure


-- * Querying

data RedlandQuery

foreign import ccall "librdf_new_query"
  librdf_new_query
  :: Ptr RedlandWorld
  -> CString
  -- ^ language name
  -> Ptr RedlandURI
  -- ^ language URI (can be NULL)
  -> CString
  -- ^ query string
  -> Ptr RedlandURI
  -- ^ base URI (can be NULL)
  -> IO (Ptr RedlandQuery)
foreign import ccall "librdf_free_query"
  librdf_free_query :: Ptr RedlandQuery -> IO ()
foreign import ccall "redland.h &librdf_free_query"
  p_librdf_free_query :: FinalizerPtr RedlandQuery

foreign import ccall "librdf_query_execute"
  librdf_query_execute
  :: Ptr RedlandQuery
  -- ^ query
  -> Ptr RedlandModel
  -- ^ model
  -> IO (Ptr RedlandQueryResults)
  -- ^ NULL on failure


-- * Query results

data RedlandQueryResults

foreign import ccall "librdf_free_query_results"
  librdf_free_query_results :: Ptr RedlandQueryResults -> IO ()
foreign import ccall "redland.h &librdf_free_query_results"
  p_librdf_free_query_results :: FinalizerPtr RedlandQueryResults

foreign import ccall "librdf_query_results_get_count"
  librdf_query_results_get_count :: Ptr RedlandQueryResults -> IO CInt
foreign import ccall "librdf_query_results_next"
  librdf_query_results_next :: Ptr RedlandQueryResults -> IO CInt
foreign import ccall "librdf_query_results_finished"
  librdf_query_results_finished :: Ptr RedlandQueryResults -> IO CInt
foreign import ccall "librdf_query_results_get_binding_value"
  librdf_query_results_get_binding_value
  :: Ptr RedlandQueryResults
  -- ^ results
  -> CInt
  -- ^ offset
  -> IO (Ptr RedlandNode)
foreign import ccall "librdf_query_results_get_binding_name"
  librdf_query_results_get_binding_name
  :: Ptr RedlandQueryResults
  -- ^ results
  -> CInt
  -- ^ offset
  -> IO CString
foreign import ccall "librdf_query_results_get_binding_value_by_name"
  librdf_query_results_get_binding_value_by_name
  :: Ptr RedlandQueryResults
  -- ^ results
  -> CString
  -- ^ variable name
  -> IO (Ptr RedlandNode)
foreign import ccall "librdf_query_results_get_bindings_count"
  librdf_query_results_get_bindings_count :: Ptr RedlandQueryResults -> IO CInt


-- * RDF Triple (librdf_statement)

data RedlandStatement

foreign import ccall "librdf_new_statement"
  librdf_new_statement :: Ptr RedlandWorld -> IO (Ptr RedlandStatement)
foreign import ccall "librdf_free_statement"
  librdf_free_statement :: Ptr RedlandStatement -> IO ()
foreign import ccall "redland.h &librdf_free_statement"
  p_librdf_free_statement :: FinalizerPtr RedlandStatement


-- * Triple stores

data RedlandStorage

foreign import ccall "librdf_new_storage"
  librdf_new_storage
  :: Ptr RedlandWorld
  -- ^ world
  -> CString
  -- ^ storage type name (e.g., "hashes")
  -> CString
  -- ^ storage identifier
  -> CString
  -- ^ options
  -> IO (Ptr RedlandStorage)
foreign import ccall "librdf_free_storage"
  librdf_free_storage :: Ptr RedlandStorage -> IO ()
foreign import ccall "redland.h &librdf_free_storage"
  p_librdf_free_storage :: FinalizerPtr RedlandStorage

foreign import ccall "librdf_new_storage_with_options"
  librdf_new_storage_with_options
  :: Ptr RedlandWorld
  -- ^ world
  -> CString
  -- ^ storage type name (e.g., "hashes")
  -> CString
  -- ^ storage identifier
  -> Ptr RedlandHash
  -- ^ options
  -> IO (Ptr RedlandStorage)


-- * Stream of triples (librdf_statement)

data RedlandStream

foreign import ccall "librdf_free_stream"
  librdf_free_stream :: Ptr RedlandStream -> IO ()
foreign import ccall "redland.h &librdf_free_stream"
  p_librdf_free_stream :: FinalizerPtr RedlandStream


-- * URI

data RedlandURI

foreign import ccall "librdf_new_uri"
  librdf_new_uri
  :: Ptr RedlandWorld
  -- ^ world
  -> CString
  -- ^ URI string
  -> IO (Ptr RedlandURI)
foreign import ccall "librdf_free_uri"
  librdf_free_uri :: Ptr RedlandURI -> IO ()
foreign import ccall "redland.h &librdf_free_uri"
  p_librdf_free_uri :: FinalizerPtr RedlandURI

foreign import ccall "librdf_new_uri_from_uri"
  librdf_new_uri_from_uri
  :: Ptr RedlandURI
  -- ^ old URI
  -> IO (Ptr RedlandURI)
foreign import ccall "librdf_uri_as_string"
  librdf_uri_as_string :: Ptr RedlandURI -> IO CString
