import Data.Default (def)
import Examples
import qualified SMTLIB.Backends.Process as Process
import SMTLIB.Backends.Tests (sources, testBackend)
import Test.Tasty

main :: IO ()
main = do
  defaultMain $
    testGroup
      "backends"
      [ testGroup
          "process"
          [ testBackend "basic examples" sources noLogging $ \todo ->
              Process.with def $ todo . Process.toBackend,
            Examples.process
          ]
      ]
  where
    noLogging = \_ _ -> return ()
