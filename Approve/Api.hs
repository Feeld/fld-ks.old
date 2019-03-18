
module Fld.Approve.Api where

import           Fld.Prelude

import           Data.Aeson            (FromJSON, ToJSON)
import           Data.Swagger          (ToParamSchema (..), ToSchema (..))
import           Data.Time.Clock.POSIX (POSIXTime)
import           Servant
import           Servant.Swagger       (toSwagger)
import           Web.HttpApiData       (FromHttpApiData, ToHttpApiData)

type ApproveApi =
         "prompt"
  :> (   ReqBody '[JSON] PromptRequest
      :> Post '[JSON] Prompt
    :<|> Capture "id" PromptId
      :> "status"
      :> Get '[JSON] PromptStatus
    :<|> QueryParam "long_poll" Bool
      :> Capture "id" PromptId
      :> Get '[JSON] Prompt
    )

_ = toSwagger (Proxy @ApproveApi)

newtype PromptId = PromptId
  { unPromptId :: Text
  }
  deriving stock Generic
  deriving newtype (Eq, Show, ToJSON, FromJSON, FromHttpApiData, ToHttpApiData)

instance ToParamSchema PromptId where
  toParamSchema _ = toParamSchema (Proxy @Text)
    -- <&> TODO: Add decorate the ParamSchema with more detailed docs here

instance ToSchema PromptId where
  declareNamedSchema _ = declareNamedSchema (Proxy @Text)
    -- <&> TODO: Add decorate the NamedSchema with more detailed docs here

data PromptRequest = PromptRequest
  { user                 :: Text
  , body                 :: Text
  , title                :: Maybe Text
  , approve_text         :: Maybe Text
  , approve_redirect_url :: Maybe Text
  , reject_text          :: Maybe Text
  , reject_redirect_url  :: Maybe Text
  , expires_in           :: Maybe NominalDiffTime
  , metadata             :: Metadata
  , long_poll            :: Maybe Bool
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

-- | Creates a 'PromptRequest' with the minimum number of fields needed
promptRequest :: Text -> Text -> PromptRequest
promptRequest user body = PromptRequest
  { user, body
  , title = Nothing
  , approve_text = Nothing
  , approve_redirect_url = Nothing
  , reject_text = Nothing
  , reject_redirect_url = Nothing
  , expires_in = Nothing
  , metadata = defaultMetadata
  , long_poll = Nothing
  }

data Prompt = Prompt
  { id         :: PromptId
  , sent_at    :: POSIXTime
  , is_expired :: Bool
  , answer     :: Maybe Answer
  -- ^ The user's answer to the ApprovalRequest. Nothing if 'long_poll' was
  -- False and the user has yet to approve the request. In this case the Prompt
  -- can be retrieved from the GET endpoint
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data Answer = Answer
  { result   :: Bool
  , time     :: POSIXTime
  , metadata :: Metadata
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

data Metadata = Metadata
  { location         :: Maybe Text
  , time             :: Maybe Text -- FIXME: de string-type this
  , ip_address       :: Maybe Text -- FIXME: de string-type this
  , browser          :: Maybe Text -- FIXME: Use sum type
  , operating_system :: Maybe Text -- FIXME Use sum type
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)

defaultMetadata :: Metadata
defaultMetadata = Metadata Nothing Nothing Nothing Nothing Nothing

data PromptStatus = PromptStatus
  { is_expired  :: Bool
  , is_answered :: Bool
  } deriving (Eq, Show, Generic, ToJSON, FromJSON, ToSchema)
