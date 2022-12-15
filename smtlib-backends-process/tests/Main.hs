import Data.Default (def)
import Examples (examples)
import qualified SMTLIB.Backends.Process as Process
import SMTLIB.Backends.Tests (sources, testBackend)
import Test.Tasty

main :: IO ()
main = do
  defaultMain $
    testGroup
      "Tests"
      [ testBackend "Basic examples" sources $ \todo ->
          Process.with def $ todo . Process.toBackend,
        testGroup "API usage examples" examples
      ]
