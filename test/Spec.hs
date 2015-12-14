import           Control.Exception (evaluate)
import           Test.Hspec
import           Test.QuickCheck

main :: IO ()
main = hspec $ do
    describe "unfinished test suite" $ do
        it "is not finished yet, so here's a dummy test" $ do
            True
