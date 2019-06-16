{-# LANGUAGE DefaultSignatures          #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase                 #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE NamedFieldPuns             #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TypeApplications           #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}
module Fld.Jwt (
  AuthResult (..)
, JwtAuth
, JwToken (..)
, JwtError (..)
, FromJwt (..)
, ToJwt (..)
, TokenDataWithExpiration (..)
, encode
, decode
, decodeWithExpiration
, generateKey
, fromRsaPEM
, toRsaPublicKeyPEM
-- * Re-exports
, MonadRandom (..)
, Jose.asPublicKey
, Jose.JWK
, Jose.KeyMaterialGenParam(..)
, Jose.Error(..)
, Jose.JWTError(..)
) where

import           Fld.Prelude

import qualified Crypto.JOSE             as Jose
import           Crypto.JOSE.JWA.JWK     (KeyMaterialGenParam (..),
                                          genKeyMaterial)
import           Crypto.JOSE.JWK         (JWK, fromKeyMaterial, fromRSA,
                                          jwkMaterial, rsaPublicKey)
import qualified Crypto.JWT              as Jose
import           Crypto.Random.Types     (MonadRandom)
import           Crypto.Store.X509       (writePubKeyFileToMemory)
import           Data.Aeson              (FromJSON (..),
                                          Result (Error, Success), ToJSON (..),
                                          fromJSON)
import qualified Data.ByteString.Char8   as BS
import qualified Data.HashMap.Strict     as HM
import qualified Data.List               as L
import qualified Data.List.NonEmpty      as NE
import           Data.Swagger            hiding (HasType)
import           Data.X509               (PrivKey (PrivKeyRSA),
                                          PubKey (PubKeyRSA))
import           Data.X509.Memory        (readKeyFileFromMemory)
import           GHC.Exts                (fromList)
import           Network.Wai             (requestHeaders)
import           Servant                 ((:>), FromHttpApiData (..),
                                          HasServer (..), MimeRender (..),
                                          MimeUnrender (..), PlainText)
import           Servant.Ekg
import           Servant.Server.Internal (DelayedIO, HasContextEntry (..),
                                          addAuthCheck, withRequest)
import           Servant.Swagger

-- | A JSON Web Token tagged with the type of the payload it contains
newtype JwToken a = JwToken { unJwToken :: ByteString }
  deriving newtype (Ord, Eq, Show, Hashable)

instance ToJSON (JwToken a) where
  toJSON = toJSON . toS @_ @Text . unJwToken

instance FromJSON (JwToken a) where
  parseJSON = fmap (JwToken . toS @Text) . parseJSON

instance MimeUnrender PlainText (JwToken a) where
  mimeUnrender ct = fmap (JwToken . toS @Text) . mimeUnrender ct

instance MimeRender PlainText (JwToken a) where
  mimeRender ct = mimeRender ct . toS @_ @Text . unJwToken

instance ToParamSchema (JwToken a) where
  toParamSchema _ = toParamSchema (Proxy @Text)

instance ToSchema (JwToken a) where
  declareNamedSchema _ = declareNamedSchema (Proxy @Text)
    <&> name ?~ "jwt"

instance FromHttpApiData (JwToken a) where
  parseHeader = pure . JwToken
  parseUrlPiece = pure . JwToken . toS

-- | Possible errors the 'encode' and 'decode' functions can throw
data JwtError
  = JoseError Jose.Error
  | JoseJWTError Jose.JWTError
  | JsonDecodeError Text
  | JwtDecodeError Text
  | NotRSA
  | NoKeys
  | ManyKeys
  deriving (Eq, Show, Exception, Generic)

instance Jose.AsError JwtError where
  _Error = prism JoseError $ \case
    JoseError e -> Right e
    other       -> Left other


-- | Encodes and signs a value as a 'JwToken'
encode
  :: ( MonadReader r m
     , HasType (NonEmpty JWK) r
     , MonadRandom m
     , MonadTime m
     , MonadError e m
     , ToJwt a
     , AsType JwtError e
     )
  => NominalDiffTime -> a -> m (JwToken a)
encode expireDiff ob = do
  expires <- addUTCTime expireDiff <$> currentTime
  key <- NE.head <$> view typed
  let claims = encodeJwt ob
        & Jose.claimExp ?~ Jose.NumericDate expires
  ejwt <- either (throwing _Typed . JoseError) pure =<< runExceptT (do
    alg <- Jose.bestJWSAlg key
    Jose.signClaims key (Jose.newJWSHeader ((), alg)) claims)
  pure $ JwToken $ toS $ Jose.encodeCompact ejwt

-- | Decodes and verifies the signature and expiration time of
-- of a 'JwToken' and returns its payload
decode
  :: ( FromJwt a
     , MonadTime m
     , MonadReader r m
     , HasType (NonEmpty JWK) r
     , MonadError e m
     , AsType JwtError e
     )
  => JwToken a -> m a
decode = fmap tokenData . decodeWithExpiration

data TokenDataWithExpiration a = TokenDataWithExpiration
  { tokenData      :: a
  , expirationDate :: UTCTime
  } deriving ( Generic, FromJSON )




-- | Decodes and verifies the signature and expiration time of
-- of a 'JwToken' and returns its payload
decodeWithExpiration
  :: ( FromJwt a
     , MonadTime m
     , MonadReader r m
     , HasType (NonEmpty JWK) r
     , MonadError e m
     , AsType JwtError e
     )
  => JwToken a -> m ( TokenDataWithExpiration a )
decodeWithExpiration (JwToken tok) = do
  keys <- view (typed @(NonEmpty JWK))
  let audienceMatches = const True --FIXME?
  verifiedJWTs <- forM keys $ \key ->
    runExceptT $ do
      unverifiedJWT <- Jose.decodeCompact (toS tok)
      Jose.verifyClaims (Jose.defaultJWTValidationSettings audienceMatches)
                        key
                        unverifiedJWT
  let ( ls, rs ) = NE.partition isLeft $ mapVerifiedJWT <$> verifiedJWTs
  let results           = NE.fromList $ rs <> ls

  either ( throwing _Typed ) return ( NE.head results )

  where
    mapVerifiedJWT ( Left e  ) = Left (JoseJWTError e)
    mapVerifiedJWT ( Right v ) = case decodeJwt v of
      Left e          -> Left (JwtDecodeError e)
      Right tokenData -> case v ^. Jose.claimExp of
        Nothing -> Left (JwtDecodeError "")
        Just ( Jose.NumericDate expirationDate )
          -> pure $ TokenDataWithExpiration{tokenData, expirationDate}


-- | Generate a random key from 'KeyMaterialGenParam'
--
-- Example: To generate a RSA keypair of 4096 bytes:
--
-- >>> key <- generateKey (RSAGenParam 4096)
--
-- 'key' contains both the private and public parts of the keypair.
-- Service which only need to verify that tokens are valid (but do not need to
-- sign them) only need the public part. To extract it:
--
-- >>> publicKey <- fromJust (key ^. asPublicKey)
--
-- Both 'key' and 'publicKey' can be de/serialized into JSON with
-- 'Data.Aeson.encode'
generateKey :: MonadRandom m => KeyMaterialGenParam -> m JWK
generateKey = fmap fromKeyMaterial . genKeyMaterial


-- | Reads a RSA 'JWK' from a PEM-encoded buffer
fromRsaPEM :: ByteString -> Either JwtError JWK
fromRsaPEM bs =
  case readKeyFileFromMemory bs of
    [PrivKeyRSA key] -> Right $ fromRSA key
    [_]              -> Left NotRSA
    []               -> Left NoKeys
    (_:_:_)          -> Left ManyKeys

toRsaPublicKeyPEM :: JWK -> Either JwtError ByteString
toRsaPublicKeyPEM k =
  case k ^. jwkMaterial of
    Jose.RSAKeyMaterial params ->
      let pk = rsaPublicKey params
      in Right $ writePubKeyFileToMemory [PubKeyRSA pk]
    _ -> Left NotRSA

-- FromJwt and ToJwt are adapted from servant-auth-server. We "vendor" them
-- here so we don't depend on it.

-- | How to decode data from a JWT.
--
-- The default implementation assumes the data is stored in the unregistered
-- @dat@ claim, and uses the @FromJSON@ instance to decode value from there.
class FromJwt a where
  decodeJwt :: Jose.ClaimsSet -> Either Text a
  default decodeJwt :: FromJSON a => Jose.ClaimsSet -> Either Text a
  decodeJwt m = case HM.lookup "dat" (m ^. Jose.unregisteredClaims) of
    Nothing -> Left "Missing 'dat' claim"
    Just v  -> case fromJSON v of
      Error e   -> Left $ toS e
      Success a -> Right a

-- | How to encode data from a Jwt.
--
-- The default implementation stores data in the unregistered @dat@ claim, and
-- uses the type's @ToJSON@ instance to encode the data.
class ToJwt a where
  encodeJwt :: a -> Jose.ClaimsSet
  default encodeJwt :: ToJSON a => a -> Jose.ClaimsSet
  encodeJwt a = Jose.addClaim "dat" (toJSON a) Jose.emptyClaimsSet

data AuthResult a
  = NotProvided
  | Invalid AuthError
  | Authenticated a
  deriving (Show, Generic, Exception)

data JwtAuth a

instance HasEndpoint (sub :: *) => HasEndpoint (JwtAuth a :> sub) where
  getEndpoint        _ = getEndpoint        (Proxy :: Proxy sub)
  enumerateEndpoints _ = enumerateEndpoints (Proxy :: Proxy sub)

instance HasSwagger api => HasSwagger (JwtAuth r :> api) where
  toSwagger _
    = toSwagger (Proxy :: Proxy api)
        & securityDefinitions <>~ fromList secs
        & allOperations.security <>~ secReqs
    where
      secs = [("JwtSecurity", scheme)]
      secReqs = [ SecurityRequirement (fromList [(s,[])]) | (s,_) <- secs]
      scheme :: SecurityScheme
      scheme =
        SecurityScheme
          (SecuritySchemeApiKey (ApiKeyParams "Authorization" ApiKeyHeader))
          (Just "The authorization header")

instance
  ( HasServer api ctx
  , HasContextEntry ctx (NonEmpty JWK)
  , FromJwt r
  ) => HasServer (JwtAuth r :> api) ctx where

  type ServerT (JwtAuth r :> api) m = AuthResult ( TokenDataWithExpiration r ) -> ServerT api m

  hoistServerWithContext _ pc nt s = hoistServerWithContext (Proxy :: Proxy api) pc nt . s

  route _ context subserver =
    route (Proxy :: Proxy api) context (subserver `addAuthCheck` authCheck)
    where
    jwks :: NonEmpty JWK
    jwks = getContextEntry context

    authCheck :: DelayedIO (AuthResult ( TokenDataWithExpiration r ))
    authCheck = withRequest $ \req -> liftIO $ do
      case L.lookup "Authorization" (requestHeaders req) of
        Just authHeader
          | Just tokenBs <- BS.stripPrefix "Bearer " authHeader -> do
            let token :: JwToken r
                token = JwToken tokenBs
            either Invalid Authenticated
              <$> runReaderT (runExceptT (decodeWithExpiration token)) ( AuthEnv jwks )
          | otherwise -> pure (Invalid InvalidHeader)
        Nothing -> pure NotProvided

newtype AuthEnv = AuthEnv
  { jwks :: NonEmpty JWK
  } deriving stock Generic

data AuthError
  = AuthJwtError JwtError
  | InvalidHeader
  deriving (Generic, Show, Eq)
