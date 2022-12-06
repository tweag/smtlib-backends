import qualified SMTLIB.Backends.Process as Process
import SMTLIB.Backends.Tests (sources, testBackend)
import Test.Tasty

main :: IO ()
main = do
  defaultMain $
    testGroup
      "backends"
      [ testBackend "process" sources noLogging $ \todo ->
          Process.with "z3" ["-in"] noLogging $ todo . Process.toBackend
      ]
  where
    noLogging = const $ return ()
