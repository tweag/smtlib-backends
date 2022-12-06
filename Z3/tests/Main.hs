import qualified SMTLIB.Backends.Z3 as Z3
import SMTLIB.Backends.Tests
import Test.Tasty

main :: IO ()
main = do
  defaultMain $
    testGroup "Tests" $
      [ testBackend "Z3" validSources noLogging $ \todo ->
          Z3.with $ todo . Z3.toBackend
      ]
  where
    noLogging = const $ return ()
    validSources = filter (\source -> name source `notElem` ["assertions", "unsat cores"]) sources
