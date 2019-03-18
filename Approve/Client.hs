module Fld.Approve.Client (
  ApproveShClientEnv
, sendRequest
, getPromptStatus
, getPrompt
, getPromptAsync

, newApproveShClientEnv
, runApproveShClientM
-- * Re-exports
, ServantError (..)
, ClientM
, module Fld.Approve.Api
) where

import           Fld.Approve.Api
import           Fld.Prelude

import           Network.HTTP.Client     (applyBasicAuth, managerModifyRequest,
                                          newManager, requestHeaders)
import           Network.HTTP.Client.TLS (tlsManagerSettings)
import           Servant
import           Servant.Client          hiding (responseHeaders)



-- | Sends a 'PromptRequest'
sendRequest :: PromptRequest -> ClientM Prompt

-- | Retrieves the 'PromptStatus' of a 'Prompt' by its 'PromptId'
getPromptStatus :: PromptId -> ClientM PromptStatus

-- | Retrieves a 'Prompt' by its 'PromptId' in a non-blocking way
getPromptAsync :: PromptId -> ClientM Prompt
getPromptAsync = getPrompt' Nothing

-- | Retrieves a 'Prompt' by its 'PromptId' in a blocking until the server
-- responds or times out
getPrompt :: PromptId -> ClientM Prompt
getPrompt = getPrompt' (Just True)


getPrompt' :: Maybe Bool -> PromptId -> ClientM Prompt

(      sendRequest
  :<|> getPromptStatus
  :<|> getPrompt'
  ) = client (Proxy @ApproveApi)



--
-- The following is specific to approve.sh
--


type ApiKey = Text

-- | We wrap the ClientEnv in a newtype to ensure we don't use it with other
-- services other than approve.sh in order not to leak the API key by mistake
newtype ApproveShClientEnv = ApproveShClientEnv
  { unApproveShClientEnv :: ClientEnv
  }

-- | Creates a 'ClientEnv' suitable for use with approve.sh
newApproveShClientEnv :: ApiKey -> IO ApproveShClientEnv
newApproveShClientEnv apiKey =
  ApproveShClientEnv
    <$> (ClientEnv
      <$> newManager settings
      <*> pure (BaseUrl Https "approve.sh" 443 "")
      <*> pure Nothing)
  where
  settings = tlsManagerSettings
    { managerModifyRequest = \req ->
        -- For some reason this function seems to be called twice so we first
        -- check that the authorization header is not already present before we
        -- add it
        pure $ if "authorization" `notElem` (map fst (requestHeaders req))
          then applyBasicAuth (toS apiKey) "" req
          else req
    }

-- | Runs a 'ClientM' action using an 'ApproveShClientEnv' as environment
runApproveShClientM
  :: ClientM a -> ApproveShClientEnv -> IO (Either ServantError a)
runApproveShClientM f = runClientM f . unApproveShClientEnv
