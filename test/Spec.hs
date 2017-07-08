import Test.Hspec
import Lib
import Data

main :: IO ()
main = hspec $ do
    describe "formatGrid" $ do 
        it "Should concatenate every line with new line" $ do
            (formatGrid ["abc", "def", "ghi"]) `shouldBe` "abc\ndef\nghi\n"

    describe "findWord" $ do
        it "Should find words that exist on the grid" $ do 
            (findWord grid "HASKELL") `shouldBe` Just "HASKELL"
            (findWord grid "CSHARP") `shouldBe` Just "CSHARP"
        it "Should return nothing for words not on the grid" $ do 
            (findWord grid "HAMSTER") `shouldBe` Nothing

    describe "findWords" $ do
        it "Should find all languages on the grid" $ do 
            (findWords grid languages ) `shouldBe` languages 