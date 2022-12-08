import Data.Default (def)
import qualified SMTLIB.Backends.Process as Process
import SMTLIB.Backends.Tests (sources, testBackend)
import Test.Tasty

main :: IO ()
main = do
  defaultMain $
    testGroup
      "backends"
      [ testBackend "process" sources def $ \todo ->
          Process.with (Process.Config "z3" ["-in"]) def $ todo . Process.toBackend
      ]
