-----------------------------------------------------------------------------
-- |
-- License     :  BSD-3-Clause
-- Maintainer  :  Oleg Grenrus <oleg.grenrus@iki.fi>
--
module GitHub.Data.Actions (
    ActionWorkflow(..),
    ActionWorkflowResult(..),
    ActionWorkflowRun(..),
    Workflow,
    ActionWorkflowRunResult(..),
    CreateWorkflowDispatchEvent(..),
    ) where

import GitHub.Data.Definitions
import GitHub.Data.Id          (Id)
import GitHub.Data.Options     (IssueState (..), MergeableState (..))
import GitHub.Data.Repos       (Repo)
import GitHub.Data.URL         (URL)
import GitHub.Internal.Prelude
import Prelude ()

import qualified Data.Text as T
import qualified Data.Vector as V

data Workflow

data ActionWorkflow = ActionWorkflow
    { 
    actionWorkflowBadgeUrl                :: !URL
    , actionWorkflowCreatedAt          :: !UTCTime
    , actionWorkflowId                 :: !(Id ActionWorkflow)
        , actionWorkflowHtmlUrl            :: !URL
    , actionWorkflowName              :: !Text
    , actionWorkflowNodeId              :: !Text
    , actionWorkflowPath              :: !Text
    , actionWorkflowState              :: !Text
    , actionWorkflowUpdatedAt          :: !UTCTime
    , actionWorkflowUrl                :: !URL
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)



data RunCommit = RunCommit
    { 
     runCommitId   :: !Text
    , runCommitTreeId   :: !Text
    }
  deriving (Show, Data, Typeable, Eq, Ord, Generic)

instance NFData RunCommit where rnf = genericRnf
instance Binary RunCommit


data ActionWorkflowRun = ActionWorkflowRun
    {
        actionWorkflowRunId :: !(Id ActionWorkflowRun)
    ,  actionWorkflowRunHeadBranch :: !Text
    ,  actionWorkflowRunHeadSha :: !Text
    , actionWorkflowRunStatus :: !Text
    , actionWorkflowRunUrl :: !URL
    , actionWorkflowRunCreatedAt :: !UTCTime
    , actionWorkflowRunUpdatedAt :: !UTCTime
    -- , actionWorkflowRunRepo :: !Repo
    , actionWorkflowRunHeadCommit :: !RunCommit
    , actionWorkflowRunConclusion :: !(Maybe Text)
    } deriving (Show, Data, Typeable, Eq, Ord, Generic)

data ActionWorkflowResult entity = ActionWorkflowResult
    {
         actionWorkflowTotalCount :: !Int
        , actionWorkflowResults    :: !(Vector entity)
    } deriving (Show, Data, Typeable, Eq, Ord, Generic)


data ActionWorkflowRunResult entity = ActionWorkflowRunResult
    {
         actionWorkflowRunResultTotalCount :: !Int
        , actionWorkflowRunResultResults   :: !(Vector entity)
    } deriving (Show, Data, Typeable, Eq, Ord, Generic)

data CreateWorkflowDispatchEvent a
    = CreateWorkflowDispatchEvent
      { createWorkflowDispatchEventRef :: !Text
      , createWorkflowDispatchEventInputs  :: !a
      }
  deriving (Show, Generic)

instance (NFData a) => NFData (CreateWorkflowDispatchEvent a) where rnf = genericRnf
instance (Binary a) => Binary (CreateWorkflowDispatchEvent a)

-------------------------------------------------------------------------------
-- JSON instances
-------------------------------------------------------------------------------

instance FromJSON ActionWorkflow where
    parseJSON = withObject "ActionWorkflow" $ \o -> ActionWorkflow
        <$> o .: "badge_url"
        <*> o .: "created_at"
        <*> o .: "id"
        <*> o .: "html_url"
        <*> o .: "name"
        <*> o .: "node_id"
        <*> o .: "path"
        <*> o .: "state"
        <*> o .: "updated_at"
        <*> o .: "url"

instance FromJSON a => FromJSON (ActionWorkflowResult a) where
    parseJSON = withObject "ActionWorkflowResult" $ \o -> ActionWorkflowResult
        <$> o .: "total_count"
        <*> o .:? "workflows" .!= V.empty

instance FromJSON a => FromJSON (ActionWorkflowRunResult a) where
    parseJSON = withObject "ActionWorkflowRunResult" $ \o -> ActionWorkflowRunResult
        <$> o .: "total_count"
        <*> o .:? "workflow_runs" .!= V.empty

instance FromJSON RunCommit where
    parseJSON = withObject "RunCommit" $ \o -> RunCommit
        <$> o .: "id"
        <*> o .: "tree_id"

instance FromJSON ActionWorkflowRun where
    parseJSON = withObject "ActionWorkflowRun" $ \o -> ActionWorkflowRun
        <$> o .: "id"
        <*> o .: "head_branch"
        <*> o .: "head_sha"
        <*> o .: "status"
        <*> o .: "url"
        <*> o .: "created_at"
        <*> o .: "updated_at"
        -- <*> o .: "repository"
        <*> o .: "head_commit"
        <*> o .:? "conclusion"


instance ToJSON a => ToJSON (CreateWorkflowDispatchEvent a) where
    toJSON (CreateWorkflowDispatchEvent ref inputs) =
        object [ "ref" .= ref, "inputs" .= inputs ]