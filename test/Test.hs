import qualified ParserNoStateTest as PNS
import           Test.Tasty

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [PNS.qcProps]
