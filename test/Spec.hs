import           Test.Tasty(defaultMain)
import           TestUtils
import qualified Text.FilePath.SubstTest as Subst

main :: IO ()
main = defaultMain testSuite

testSuite :: TestTree
testSuite = Subst.testSuite
