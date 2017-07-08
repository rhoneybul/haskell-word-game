Use Test.Hspec
import Lib

main = hspec $ do
	describe "How to run a test" $ do 
		it "Should be able to run a test" $ do
			someString `shouldBe` "Hello, Words."

