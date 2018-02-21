{- |
Module      :  Redland.Util
Maintainer  :  defanor <defanor@uberspace.net>
Stability   :  unstable
Portability :  non-portable (GHC extensions are used)

Utility functions based on mid-level bindings.

-}

module Redland.Util where

import Foreign
import Control.Exception

import Redland.LowLevel
import Redland.MidLevel


-- * Hashes

withHash :: ForeignPtr RedlandWorld
         -> String
         -> [(String, String)]
         -> (ForeignPtr RedlandHash -> IO a)
         -> IO a
withHash world factory l f =
  withNew (redlandHash world factory) $ \hash -> do
  mapM_ (uncurry (hashPutStrings hash)) l
  f hash


-- * RDF term (librdf_node)

data Node = BlankNode String
          | LiteralNode String
          | ResourceNode String
          deriving (Eq, Show)

redlandNodeToNode :: ForeignPtr RedlandNode -> IO Node
redlandNodeToNode rn = do
  isBlank <- nodeIsBlank rn
  isLiteral <- nodeIsLiteral rn
  isResource <- nodeIsResource rn
  case (isBlank, isLiteral, isResource) of
    (True, _, _) -> BlankNode <$> nodeGetBlankIdentifier rn
    (_, True, _) -> LiteralNode <$> nodeGetLiteralValue rn
    _ -> ResourceNode <$> (nodeGetURI rn >>= uriAsString)


-- * Parsers

-- | Tries different parsers until one of them succeeds.
tryParseStringIntoModel :: ForeignPtr RedlandWorld
                        -> [String]
                        -- ^ parsers to try
                        -> ForeignPtr RedlandModel
                        -> ForeignPtr RedlandURI
                        -- ^ base URI
                        -> String
                        -- ^ string to parse
                        -> IO ()
tryParseStringIntoModel world (parser:parsers) model uri str =
  handle tryNext $
  withNew (redlandParser world (Just parser) Nothing Nothing) $ \parser ->
  parseStringIntoModel parser str uri model
  where
    tryNext :: RedlandException -> IO ()
    tryNext (OperationException _) = tryParseStringIntoModel world parsers model uri str
    tryNext e = throw e
tryParseStringIntoModel world [] model uri str = throw ParseFailure


-- * Querying

withQuery :: ForeignPtr RedlandWorld
          -> ForeignPtr RedlandModel
          -> String
          -- ^ query language
          -> String
          -- ^ query string
          -> Maybe (ForeignPtr RedlandURI)
          -- ^ base URI
          -> (QueryResults -> IO a)
          -> IO a
withQuery world model ql qs bURI f =
  withNew (redlandQuery world ql Nothing qs bURI) $ \query ->
  withNew (modelQueryExecute model query) $ \results ->
  queryResultsToList results >>= f


-- * Query results

type QueryResults = [[(String, Node)]]

queryResultsToList :: ForeignPtr RedlandQueryResults -> IO QueryResults
queryResultsToList qr = do
  done <- queryResultsFinished qr
  if done
    then pure []
    else do
    bindingCnt <- queryResultsGetBindingsCount qr
    bindings <- mapM readBinding [0..bindingCnt - 1]
    next <- queryResultsNext qr
    rest <- if next then queryResultsToList qr else pure []
    pure (bindings : rest)
      where
        readBinding :: Int -> IO (String, Node)
        readBinding n = do
          name <- queryResultsGetBindingName qr n
          val <- queryResultsGetBindingValue qr n >>= redlandNodeToNode
          pure (name, val)


-- * Other

-- | Initializes world, storage, model, and base URI at once.
withWSMU :: String
        -- ^ storage factory
        -> [(String, String)]
        -- ^ storage options
        -> String
        -- ^ storage identifier
        -> String
        -- ^ model options
        -> String
        -- ^ base URI
        -> (ForeignPtr RedlandWorld ->
             ForeignPtr RedlandStorage ->
             ForeignPtr RedlandModel ->
             ForeignPtr RedlandURI ->
             IO a)
        -> IO a
withWSMU sFactory sOpt sIdent mOpt bURI f =
  withNew redlandWorld $ \world ->
  withHash world "memory" sOpt $ \sOpt' ->
  withNew (redlandStorageWithOptions world sFactory sIdent sOpt') $ \storage ->
  withNew (redlandModel world storage mOpt) $ \model ->
  withNew (redlandURI world bURI) $ \uri ->
  f world storage model uri
