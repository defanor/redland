{- |
Module      :  Redland.Util
Maintainer  :  defanor <defanor@uberspace.net>
Stability   :  unstable
Portability :  non-portable (GHC extensions are used)

Utility functions based on mid-level bindings.

-}

module Redland.Util where

import Foreign
import Control.Monad
import Control.DeepSeq

import Redland.LowLevel
import Redland.MidLevel


-- * Hashes

-- | Mostly a conversion function.
withHash :: ForeignPtr RedlandWorld
         -> String
         -> [(String, String)]
         -> (ForeignPtr RedlandHash -> IO a)
         -> IO a
withHash world factory l f =
  withNew (redlandHash world factory) $ \hash -> do
  mapM_ (uncurry (hashPutStrings hash)) l
  f hash


-- * RDF Graph (librdf_model)

-- | Wrapper around 'modelFindStatements'.
withStatements :: ForeignPtr RedlandWorld
               -> ForeignPtr RedlandModel
               -> Triple
               -> ([Triple] -> IO a)
               -> IO a
withStatements world model t f =
  withNew (tripleToStatement world t) $ \statement ->
  withNew (modelFindStatements model statement) $
  streamToList >=> f


-- * RDF term (librdf_node)

data LiteralNodeType = XMLSchema String
                     | LanguageTag String
                     deriving (Ord, Eq, Show)

instance NFData LiteralNodeType where
  rnf (XMLSchema s) = rnf s
  rnf (LanguageTag s) = rnf s

-- | Haskell representation of 'RedlandNode'.
data Node = BlankNode String
          | LiteralNode String (Maybe LiteralNodeType)
          | ResourceNode String
          deriving (Ord, Eq, Show)

instance NFData Node where
  rnf (BlankNode s) = rnf s
  rnf (LiteralNode s t) = rnf s `seq` rnf t
  rnf (ResourceNode s) = rnf s

-- | A conversion function.
redlandNodeToNode :: ForeignPtr RedlandNode -> IO Node
redlandNodeToNode rn = do
  isBlank <- nodeIsBlank rn
  isLiteral <- nodeIsLiteral rn
  isResource <- nodeIsResource rn
  case (isBlank, isLiteral, isResource) of
    (True, _, _) -> BlankNode <$> nodeGetBlankIdentifier rn
    (_, True, _) -> do
      litVal <- nodeGetLiteralValue rn
      litLang <- nodeGetLiteralValueLanguage rn
      litType <- nodeGetLiteralValueDatatypeURI rn
      nType <- case (litLang, litType) of
        (Just l, _) -> pure $ Just $ LanguageTag l
        (_, Just t) -> Just . XMLSchema <$> uriAsString t
        _ -> pure Nothing
      pure $ LiteralNode litVal nType
    _ -> ResourceNode <$> withNew (nodeGetURI rn) uriAsString

-- | A conversion function.
nodeToRedlandNode :: ForeignPtr RedlandWorld
                  -> Node
                  -> Initializer RedlandNode
nodeToRedlandNode world (BlankNode s) = nodeFromBlankIdentifier world (Just s)
nodeToRedlandNode world (LiteralNode s (Just (LanguageTag l))) =
  nodeFromTypedLiteral world s (Just l) Nothing
nodeToRedlandNode world (LiteralNode s (Just (XMLSchema uri))) =
  withNew (redlandURI world uri) $ \uri' ->
  nodeFromTypedLiteral world s Nothing (Just uri')
nodeToRedlandNode world (LiteralNode s Nothing) =
  nodeFromTypedLiteral world s Nothing Nothing
nodeToRedlandNode world (ResourceNode s) =
  withNew (redlandURI world s) $ nodeFromURI world


-- * Parsers

-- | Guesses a parser name, and applies it.
guessingParseStringIntoModel :: ForeignPtr RedlandWorld
                             -> ForeignPtr RedlandModel
                             -> ForeignPtr RedlandURI
                             -- ^ base URI
                             -> String
                             -- ^ string to parse
                             -> IO ()
guessingParseStringIntoModel world model uri str = do
  parserName <- parserGuessName2 world Nothing (Just str) Nothing
  withNew (redlandParser world parserName Nothing Nothing) $ \p ->
    parseStringIntoModel p str uri model


-- * Querying

-- | Querying helper.
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
  withNew (modelQueryExecute model query) $ queryResultsToList >=> f


-- * Query results

type QueryResults = [[(String, Node)]]

-- | A conversion function.
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


-- * RDF Triple (librdf_statement)

-- | Haskell representation of 'RedlandStatement'.
data Triple = Triple { subject :: Maybe Node
                     , predicate :: Maybe Node
                     , object :: Maybe Node
                     } deriving (Ord, Eq, Show)

instance NFData Triple where
  rnf (Triple s p o) = rnf s `seq` rnf p `seq` rnf o

-- | A conversion function.
statementToTriple :: ForeignPtr RedlandStatement
                  -> IO Triple
statementToTriple statement = do
  s <- componentToTriple statementGetSubject
  p <- componentToTriple statementGetPredicate
  o <- componentToTriple statementGetObject
  pure $ Triple s p o
  where
    componentToTriple :: (ForeignPtr RedlandStatement ->
                          InitializerMaybe RedlandNode)
                      -> IO (Maybe Node)
    componentToTriple f = withNewMaybe (f statement) redlandNodeToNode

-- | A conversion function.
tripleToStatement :: ForeignPtr RedlandWorld
                  -> Triple
                  -> Initializer RedlandStatement
tripleToStatement world (Triple s p o) = do
  statement <- redlandStatement world
  let maybeSet f mn = case mn of
        Just n -> withNew (nodeToRedlandNode world n) $ \n' ->
          f statement (Just n')
        Nothing -> pure ()
  maybeSet statementSetSubject s
  maybeSet statementSetPredicate p
  maybeSet statementSetObject o
  pure statement

-- * Stream of triples (librdf_statement)

-- | A conversion function.
streamToList :: ForeignPtr RedlandStream -> IO [Triple]
streamToList stream = do
  done <- streamEnd stream
  if done
    then pure []
    else withNew (streamGetObject stream) $ \statement -> do
    triple <- statementToTriple statement
    next <- streamNext stream
    rest <- if next then streamToList stream else pure []
    pure (triple : rest)


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
