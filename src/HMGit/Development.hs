{-# LANGUAGE TemplateHaskell #-}
module HMGit.Development (
    gitCfg
) where

import           HMGit.Development.TH
import           HMGit.Internal.Core.Runner (HMGitConfig (..))

import           Path                       (Abs, Dir, mkAbsDir)

gitCfg :: HMGitConfig
gitCfg = HMGitConfig {
    hmGitDir = $(mkAbsDir $(relativeProjRoot ".git"))
  , hmGitTreeLimit = 1000
  }
