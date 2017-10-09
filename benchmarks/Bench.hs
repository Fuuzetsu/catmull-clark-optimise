module Main (main) where

import           Criterion (bench, bgroup, nf)
import           Criterion.Main (defaultConfig, defaultMainWith)
import           Criterion.Types (Config(reportFile))
import qualified Mesh.V0 as V0
import qualified Mesh.V1 as V1
import qualified Mesh.V2 as V2
import qualified Mesh.V3 as V3
import qualified Mesh.V4 as V4

config :: Config
config = defaultConfig
  { reportFile = Just "benchmarks.html"
  }

main :: IO ()
main = defaultMainWith config
  [ bgroup "main"
    [ bench "V0" $ nf V0.main 3
    , bench "V1" $ nf V1.main 3
    , bench "V2" $ nf V2.main 3
    , bench "V3" $ nf V3.main 3
    , bench "V4" $ nf V4.main 3
    ]
  ]
