{-# LANGUAGE TemplateHaskell #-}
module GHCi (
    defCfg
) where
import           HMGit.Development          (relativeProjRoot)
import           HMGit.Internal.Core.Runner
import           Path

defCfg :: HMGitConfig
defCfg = HMGitConfig {
    hmGitDir = $(mkAbsDir $(relativeProjRoot "./"))
  , hmGitTreeLimit = 1000
  }
