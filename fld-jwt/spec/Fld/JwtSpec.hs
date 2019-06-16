{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DerivingStrategies         #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
module Fld.JwtSpec (main, spec) where

import qualified Fld.Jwt               as Jwt
import           Fld.Prelude

import           Data.Aeson            (FromJSON, ToJSON)
import           NeatInterpolation     (text)
import           Test.Hspec            (Spec, describe, hspec, runIO, shouldBe)
import           Test.Hspec.QuickCheck (prop)
import           Test.QuickCheck       (Arbitrary, Positive (..))

main :: IO ()
main = hspec spec

spec :: Spec
spec =
  describe "Asymmetric keys (RSA)" $ do
    -- WARNING: This is a small RSA key. We keep it small so tests run
    -- a bit faster. DO NOT USE SUCH AS SMALL KEY IN PRODUCTION!
    oldKey <- runIO $ Jwt.generateKey (Jwt.RSAGenParam 256)
    newKey <- runIO $ Jwt.generateKey (Jwt.RSAGenParam 256)

    let generatedKeys = newKey :| [ oldKey ]

    describe "generated key" $ rsaSpec generatedKeys
    describe "key loaded from PEM" $ rsaSpec $ rsaPemKey :| []
    describe "env with multiple keys" $ multiKeysSpec newKey oldKey

rsaSpec :: NonEmpty Jwt.JWK -> Spec
rsaSpec keys@(key :| _ ) = do

    prop "encode/decode are reciprocal" $ \(payload, Positive expireDiff, time) -> do
      eDecoded :: Either Error Payload <- runExceptT $
        runReaderT (Jwt.decode =<< Jwt.encode expireDiff payload) (Env keys time)
      eDecoded `shouldBe` Right payload

    prop "expired token throws error" $ \(payload, Positive expireDiff, time) -> do
      eDecoded :: Either Error Payload <- runExceptT $
        runReaderT (Jwt.decode =<< Jwt.encode (negate expireDiff) payload) (Env keys time)
      eDecoded `shouldBe` Left (Error (Jwt.JoseJWTError Jwt.JWTExpired))

    prop "can extract public key and use it to verify" $ \(payload, Positive expireDiff, time) -> do
      eDecoded :: Either Error Payload <- runExceptT $ do
        encoded <- runReaderT (Jwt.encode expireDiff payload) (Env keys time)
        let Just publicKey = key ^. Jwt.asPublicKey
        runReaderT (Jwt.decode encoded) (Env ( publicKey :| [] ) time)
      eDecoded `shouldBe` Right payload

    prop "cannot sign with public key" $ \(payload, Positive expireDiff, time) -> do
      let Just publicKey = key ^. Jwt.asPublicKey
      eEncoded :: Either Error (Jwt.JwToken Payload) <- runExceptT $
        runReaderT (Jwt.encode expireDiff payload) (Env ( publicKey :| [] ) time)
      eEncoded `shouldBe` Left (Error (Jwt.JoseError (Jwt.KeyMismatch "not an RSA private key")))

multiKeysSpec :: Jwt.JWK -> Jwt.JWK -> Spec
multiKeysSpec newKey oldKey =

    prop "can decode payload encoded with secondary keys that are in the env "
    $ \(payload, Positive expireDiff, time) -> do

      let envWithOldKeyOnly = Env ( oldKey :| [] ) time
      let envWithNewKeyOnly = Env ( newKey :| [] ) time
      let envWithAllKeys    = Env ( newKey :| [ oldKey ] ) time

      Right encodedOldKey
        :: Either Error ( Jwt.JwToken Payload )
        <- runExceptT
         $ runReaderT ( Jwt.encode expireDiff payload ) envWithOldKeyOnly

      Left err
        :: Either Error Payload
        <- runExceptT $ runReaderT (Jwt.decode encodedOldKey) envWithNewKeyOnly

      err `shouldBe` (Error $ Jwt.JoseJWTError
                       (Jwt.JWSError Jwt.JWSInvalidSignature))

      Right decodedPayload
        :: Either Error Payload
        <- runExceptT $ runReaderT (Jwt.decode encodedOldKey) envWithAllKeys

      decodedPayload `shouldBe` payload


data Env = Env
  { jwks    :: NonEmpty Jwt.JWK
  , curTime :: UTCTime
  } deriving Generic

newtype Error = Error Jwt.JwtError
  deriving newtype (Show, Eq)
  deriving stock Generic

instance Monad m => MonadTime (ReaderT Env m) where
  currentTime = asks curTime

newtype Payload = Payload Int
  deriving newtype (Eq, Show, Arbitrary, ToJSON, FromJSON)
  deriving stock Generic
  deriving anyclass (Jwt.ToJwt, Jwt.FromJwt)

rsaPemKey :: Jwt.JWK
Right rsaPemKey = Jwt.fromRsaPEM $ toS [text|
  -----BEGIN RSA PRIVATE KEY-----
  MIIEpAIBAAKCAQEArMqEFuDF4mVqVxn1hQdYm0JTqNdf4iubu4eSKZLs/dRVRXFv
  UhNgBmIBk3/drdK8dOTeiMw/QPAbvGK8Sq7jiDNcKonq1nBAOkAUB8rcptf8yfob
  lG24NEFcujoHTjH0grR5VgVnUQMZ2CabKOVlVfJ2YCwdofxl76OHQFfE9WRlwoLx
  hGIx2x8S5gqpY0rduNtI/KDIGvqiG+zuftvRLrurtmdLsALovc2LO59hPadrobfk
  ArU6jXAryntVXyUrrw1KPXT3dsnKnVqQfzy1W1Rh3ZmsGaOft8xBZDXEtTWJ5dHw
  /m5QmNEu77RqKH0SAhr9iiOt4jk6WQvcmvZgQQIDAQABAoIBAQCOaa+qp4GRUiLu
  PSAFj4NxG56WN0T4xRZq3pcSGt2na10IijDFBeEMAbTo2+daf6kB8yyStTyhCgTi
  JMKqDzDLLgGVsexS51uYZv3bWlC4VqYr1i7rC+9ZalHBz1f8E+JB4/tpcbSwqYyg
  gVk6wK7fwqHcLCAURVHCzBYKawyTbtxZ4edk6Lj3mHShsFI8upgZ2JrEVvSacsCh
  RPJ5OdhhcmskwwcjIfO1APHen4t4dKLMV4fzG8wExo7K5HD2RT2kf8TwNouuZbp1
  ikzT1ky9o/q3O2H83ZY+mqMTZb8ALRPnn6jW5ebryHWZi0V747kVe9mw6QfRpB0O
  LVYlt8t9AoGBANMygZk7qPII+J6oMOBhV/bPfwpY6vPMErZsmM0wx4ZApHZwRqYw
  MJitL18yoFeY7rd5QWb7X+kYPaENisLKqvT3q3Bxkuxx5VlbG0dE+nJivZigOmGZ
  czhd8uAc56LlzXJfvnXL5BCCQ7UKqwUiQXOEWGMDCORiAh3UmKvmf/a/AoGBANFy
  SHpB1PryMjnI7LNZJAz2WjysQvIqKNTYcfTeCas0j+ZEUiyHi98XanS9bcwth3qX
  1ncQPDBtEQu1uahV1xe3jQeTGP9GbCRtPCReUNZbooO05mk0z4A8ne9WfvN0STut
  bzEPimXrb7EZdv5hwnmNG1MnNVzC1AVdKbt9Pmj/AoGBAJ5/qQN24rPCR9PKJeQ/
  KeTwlEcPtESDQwflOEfF8/etS8AXQu41/EKYtpSbSfpzq7PGLG88+B3XM2lK4HiP
  4P03YlJg6gLI93a4CSDTg6GUuPVySl4062PCHiRTjQOTszvC3w1Il0BMgsO73d1c
  eAJZsHueK246n5wDkRBxTFfBAoGAAtqYzDmrakP0lT78orD7QcX1ktASFXSnm8bb
  Pqfr385Wi5+wsvSh3kdWZwLRxS91oiEBhhvNhidmTQ+L2RRaWvNgU5MYixntjU2D
  vUcfuNwEyeCkJxjWHvw0drdqAW07Njn1wuhgTWU432PmcC7q1HcswSROoNXyx6Pu
  VqStPgcCgYBD67ZxuynJ9c3vg3FNy+QOdwo2SqAY2u2Inb6f0vI0eHnMK9JCGN1B
  ef1Tup2xptoqX37RTmqVJ8lkQqvp53VgdEenZh/gNRRiMAgb2e6FsREVEyqHt9ch
  sZ6ogkWHFOYmODEf/DHLa5CZLaCU44CjSmYnE+6HufLkmGshdcTHPg==
  -----END RSA PRIVATE KEY-----
  |]
