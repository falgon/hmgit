module HMGit.Commands.Porcelain.Init (
    init
  , Init (..)
  , initCmd
) where

import           HMGit.Commands.Porcelain.Init.Cmd  (initCmd)
import           HMGit.Commands.Porcelain.Init.Core (Init (..), init)

import           Prelude                            hiding (init)
