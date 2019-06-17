{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE UndecidableInstances       #-}
{-|

Module      : Fld.Postgres.Typed

This module provides postgres access typeclasses, types and functions.
-}
module Fld.Postgres.Typed (
  module Fld.Postgres.Typed
-- * Re-exports
, PGConnection
, PGError
, PGQuery
, dataPGEnum
, dataPGRelation
, pgErrorCode
, pgSQL
, module ReExport
) where

import           Fld.Prelude

import           Control.Monad.Logger               (MonadLogger, logWarn)
import           Data.Aeson                         (FromJSON, ToJSON)
import           Data.Pool                          (Pool, createPool,
                                                     destroyAllResources,
                                                     destroyResource,
                                                     putResource, takeResource)
import           Database.PostgreSQL.Typed          (PGConnection,
                                                     PGDatabase (..), PGError,
                                                     defaultPGDatabase,
                                                     pgConnect, pgDisconnect,
                                                     pgExecute, pgQuery, pgSQL)
import           Database.PostgreSQL.Typed.Enum     (dataPGEnum)
import           Database.PostgreSQL.Typed.ErrCodes as ReExport
import           Database.PostgreSQL.Typed.Protocol (PGTlsMode (..),
                                                     PGTlsValidateMode (..),
                                                     pgBegin, pgCommit,
                                                     pgErrorCode, pgRollback,
                                                     pgTlsValidate)
import           Database.PostgreSQL.Typed.Query    (PGQuery)
import           Database.PostgreSQL.Typed.Relation (dataPGRelation)
import qualified Prelude
import           System.Envy                        (FromEnv (..), envMaybe,
                                                     (.!=))

import           Network.Socket                     (SockAddr (SockAddrUnix))

-- | The configuration of the database connection pool
data PoolConfig = PoolConfig
  { stripes     :: !Int
  , size        :: !Int
  , idleTimeout :: !NominalDiffTime
  } deriving (Eq, Show, Generic, ToJSON, FromJSON)

instance FromEnv PoolConfig where
  fromEnv = do
    stripes <- envMaybe "PG_POOL_STRIPES" .!= 1
    size <- envMaybe "PG_POOL_SIZE" .!= 1
    idleTimeout <- realToFrac <$> (envMaybe @Double "PG_POOL_IDLE_TIMEOUT" .!= 1)
    pure PoolConfig {stripes,size,idleTimeout}

newtype PGDatabaseConfig = PGDatabaseConfig PGDatabase

-- | Based on getTPGDatabase but implemented here for doc. purposes
instance FromEnv PGDatabaseConfig where
  fromEnv = do
    user <- ((<|>) <$> envMaybe "TPG_USER" <*> envMaybe "USER") .!= "postgres"
    db   <- envMaybe "TPG_DB" .!= user
    host <- envMaybe "TPG_HOST" .!= "localhost"
    port <- envMaybe "TPG_PORT" .!= "5432"
    msocket <- envMaybe "TPG_SOCK"
    passwd <- envMaybe "TPG_PASS" .!= ""
    debug <- isJust <$> envMaybe @Text "TPG_DEBUG"
    useTLS <- isJust <$> envMaybe @Text "TPG_TLS"
    tlsVerifyMode <- envMaybe @Text "TPG_TLS_MODE" >>= \case
      Just "full" -> pure TlsValidateFull
      Just "ca"   -> pure TlsValidateCA
      Just other  -> throwError $ toS $ "Unknown verify mode: " <> other
      Nothing     -> pure TlsValidateCA
    mTlsCertPem <- envMaybe @Text "TPG_TLS_ROOT_CERT"
    pgDBTLS <- case mTlsCertPem of
      Just certPem     -> either throwError pure (pgTlsValidate tlsVerifyMode (toS certPem))
      Nothing | useTLS -> pure TlsNoValidate
      Nothing          -> pure TlsDisabled
    pure $ PGDatabaseConfig $ defaultPGDatabase
      { pgDBAddr = case msocket of
          Nothing   -> Left (host,port)
          Just sock -> Right (SockAddrUnix sock)
      , pgDBName = db
      , pgDBUser = user
      , pgDBPass = passwd
      , pgDBDebug = debug
      , pgDBTLS
      }

-- | A database connection pool
newtype PGConnectionPool = PGConnectionPool
  { unPGConnectionPool :: Pool PGConnection }


-- | Initializes a database connection pool, passes it to the continuation and
-- makes sure it is destroyed after the continuation is done, even in the case
-- of exceptions
withPool
  :: ( MonadUnliftIO m
     , MonadMask m
     , MonadLogger m
     )
  => PoolConfig -> PGDatabaseConfig -> (PGConnectionPool -> m a) -> m a
withPool poolCfg (PGDatabaseConfig dbCfg) f =
  bracket (initializePool poolCfg dbCfg) finalizePool $ \pool -> do
    async $
      withConnection pool smokeTestQuery
        `catch` \(e :: SomeException) ->
          $(logWarn) $ "Could not perform smoke-test query: " <> show e
    f pool

-- | Runs a dummy query to make sure the connection parameters are valid
-- and that the database is alive.
-- If done at init time and DB cannot be reached it will crash the process to
-- fail-fast and ensure a properly initialized app implies the db is alive
-- and properly configured
smokeTestQuery :: MonadPG (ReaderT r m) => r -> m ()
smokeTestQuery = runReaderT (void $ dbQuery ("SELECT 1" :: ByteString))

-- | Takes a connection from pool, passes it
-- to the continuation and makes sure the connection is returned to the pool
-- after the continuation is done, even in the case of exceptions.
withConnection :: (MonadIO m, MonadMask m) => PGConnectionPool -> (PGConnection -> m a) -> m a
withConnection (PGConnectionPool pool) = withResource pool

withConnectionNoPool
  :: ( MonadUnliftIO m
     , MonadMask m
     , MonadLogger m
     )
  => PGDatabaseConfig -> (PGConnection -> m a) -> m a
withConnectionNoPool (PGDatabaseConfig dbCfg) f =
  bracket (liftIO $ pgConnect dbCfg) (liftIO . pgDisconnect) $ \conn -> do
    async $
      smokeTestQuery conn
        `catch` \(e :: SomeException) ->
          $(logWarn) $ "Could not perform smoke-test query: " <> show e
    f conn

-- | This is equivalent to `Data.Pool.withResource' but usable with any
-- 'MonadMask'
-- FIXME: This is not tied to postgres at all so we might want to move it
-- somewhere else if/when we need it for other 'Pool's
withResource :: (MonadIO m, MonadMask m) => Pool a -> (a -> m b) -> m b
withResource pool act = mask $ \restore -> do
  (resource, localPool) <- liftIO (takeResource pool)
  ret <- restore (act resource) `onException` liftIO (destroyResource pool localPool resource)
  liftIO (putResource localPool resource)
  return ret

-- | Initializes a database connection pool
initializePool :: MonadIO m => PoolConfig -> PGDatabase -> m PGConnectionPool
initializePool PoolConfig{stripes,idleTimeout,size} dbCfg =
  PGConnectionPool <$> liftIO (createPool (pgConnect dbCfg) pgDisconnect stripes idleTimeout size)

-- | Destroys a database connection pool
finalizePool :: MonadIO m => PGConnectionPool -> m ()
finalizePool = liftIO . destroyAllResources . unPGConnectionPool

class Monad m => MonadPG m where
  -- | Runs a database query which does not return results. Returns the number of
  -- rows affected or -1 if unknown
  dbExecute :: PGQuery q () => q -> m Int

  -- | Runs a database query which may return multiple rows
  dbQuery :: PGQuery q a => q -> m [a]

  -- | Begins a transaction
  dbBegin :: m ()

  -- | Commits a transaction
  dbCommit :: m ()

  -- | Rolls back a transaction
  dbRollback :: m ()

instance MonadIO m => MonadPG (ReaderT PGConnection m) where
  dbExecute q = liftIO . flip pgExecute q =<< ask
  dbQuery q = liftIO . flip pgQuery q =<< ask
  dbBegin = liftIO . pgBegin =<< ask
  dbCommit = liftIO . pgCommit =<< ask
  dbRollback = liftIO . pgRollback =<< ask

-- | Like 'dbExecute' but ignores the return value
dbExecute_ :: (MonadPG m, PGQuery q ()) => q -> m ()
dbExecute_ = void . dbExecute

-- | Like 'dbQuery' but for queries which we know that return at most 1 row
-- WARNING: Throws an 'UserError' if more than 1 row is returned, use with care!
-- FIXME: Use 'MonadError' instead of 'Prelude.fail'
dbQuery1 :: (MonadPG m, PGQuery q a, Show q) => q -> m (Maybe a)
dbQuery1 q = do
  r <- dbQuery q
  case r of
    []  -> return Nothing
    [x] -> return $ Just x
    _   -> Prelude.fail $ "pgQuery1 " ++ show q ++ ": too many results"

-- | Like 'dbQuery1' but the query must return exactly 1 result, else an
-- 'UserError' is thrown. Use with care
-- FIXME: Use 'MonadError' instead of 'Prelude.fail'
dbQuery1' :: (MonadPG m, PGQuery q a, Show q) => q -> m a
dbQuery1' q = maybe (Prelude.fail $ "pgQuery1' " ++ show q ++ ": no results") return =<< dbQuery1 q

-- | Wraps a MonadPG action in a transaction, committing it on success
-- and rolling it back on exception
dbTransaction
  :: ( MonadPG m
     , MonadMask m
     )
  => m a -> m a
dbTransaction f = do
  dbBegin
  (do r <- f
      dbCommit
      return r
    ) `onException` dbRollback

-- | Wraps a 'MonadPG' action in a transaction which will be unconditionally
-- rolled back. Useful for tests.
dbDryRun
  :: ( MonadPG m
     , MonadMask m
     )
  => m a -> m a
dbDryRun f = do
  dbBegin
  f `finally` dbRollback

-- | Class of monads that can run a 'MonadPG' action. We want this so we can use
-- 'runDB' in server handlers, tests, etc..
class MonadPG (RunDBM m) => HasRunDB m where
  -- | 'RunDBM' is the associated monad that is lifted into 'm'
  type RunDBM m :: * -> *
  -- | provide a default for the common case of threading the connection with a
  -- 'ReaderT'
  type RunDBM m =  ReaderT PGConnection m
  runDB :: RunDBM m a -> m a


-- | An implementation of 'runDB' that runs the 'MonadPG' in a transaction.
-- It is designed to play well with 'MonadError' and also rolls back the
-- transaction if the error flows through 'MonadError' instead of being thrown as
-- a 'MonadThrow' exception
newtype RunDBPoolTransaction m a = RunDBPoolTransaction (m a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadIO
    , MonadReader r
    , MonadError e
    )

instance MonadTrans RunDBPoolTransaction
  where lift = RunDBPoolTransaction

instance
  ( MonadIO m
  , MonadMask m
  , MonadReader r m
  , MonadError e m
  , Exception e
  , HasType PGConnectionPool r
  ) => HasRunDB (RunDBPoolTransaction m) where
  runDB = runDBPool

runDBPool
  :: ( MonadMask m
     , MonadIO m
     , MonadReader r m
     , MonadError e m
     , Exception e
     , HasType PGConnectionPool r
     )
  =>  ReaderT PGConnection m a -> m a
runDBPool f = do
  pool <- view typed
  withConnection pool (runReaderT (dbTransaction (f `catchError` throwM)))
   `catch` throwError

-- | An implementation of 'runDB' which runs the 'MonadPG' in a transaction
-- which will be unconditionally rolled back.
-- This is useful in test environments where we want access to a real postgres
-- database but we want to keep each test isolated from each other and don't leave
-- rubbish behind
newtype RunDBPoolDryTransaction m a = RunDBPoolDryTransaction (m a)
  deriving newtype
    ( Functor
    , Applicative
    , Monad
    , MonadThrow
    , MonadCatch
    , MonadMask
    , MonadIO
    , MonadReader r
    , MonadError e
    )

instance
  ( MonadIO m
  , MonadMask m
  , MonadReader r m
  , HasType PGConnectionPool r
  ) => HasRunDB (RunDBPoolDryTransaction m) where
  runDB = dryRunDBPool

dryRunDBPool
  :: ( MonadMask m
     , MonadIO m
     , MonadReader r m
     , HasType PGConnectionPool r
     )
  =>  ReaderT PGConnection m a -> m a
dryRunDBPool f = do
  pool <- view typed
  withConnection pool (runReaderT (dbDryRun f))
