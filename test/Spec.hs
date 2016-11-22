import Test.Hspec
import Data.Maybe
import Models
import Strategy

main :: IO ()
main = hspec $ do
  describe "Strategy" $ do
    it "returns (1,1) as the first move" $ do
      firstMove 'x' `shouldBe` Move 1 1 'x'

    it "returns false if there is no winner" $ do
      winnerExists [Move 0 0 'x', Move 2 2 'o', Move 1 1 'x', Move 2 0 'o', Move 1 2 'x'] `shouldBe` False

    it "returns true if winner exists" $ do
      winnerExists [Move 0 0 'x', Move 1 0 'x', Move 2 0 'x', Move 0 2 'o', Move 2 2 'o'] `shouldBe` True

    it "second move is in the one of the corners" $ do
      move <- nextMove 'o' [Move 1 1 'x']
      (fromJust move) `elem` (allCornerMoves 'o') `shouldBe` True
    
    it "puts last if only one left for us" $ do
      move <- nextMove 'x' [Move 1 1 'x', Move 0 1 'o', Move 0 0 'x', Move 2 1 'o']
      (fromJust move) `shouldBe` Move 2 2 'x'

    it "try to defend if only one left for the opponent" $ do
      move <- nextMove 'o' [Move 1 1 'x', Move 0 1 'o', Move 0 0 'x']
      (fromJust move) `shouldBe` Move 2 2 'o'

    it "puts in the same line if there is a empty line" $ do
      move <- nextMove 'o' [Move 1 1 'x', Move 0 0 'o', Move 2 2 'x']
      (fromJust move) `elem` [Move 0 1 'o', Move 0 2 'o', Move 1 0 'o', Move 2 0 'o'] `shouldBe` True

    it "puts randomly in the free space if nothing else matches" $ do
      move <- nextMove 'o' [Move 1 1 'x', Move 2 1 'o', Move 0 1 'x', Move 0 0 'o', Move 2 0 'x', Move 0 2 'o', Move 2 2 'x']
      (fromJust move) `elem` [Move 1 0 'o', Move 1 2 'o'] `shouldBe` True

    it "returns Nothing if board is full" $ do
      move <- nextMove 'o' [Move 1 1 'x', Move 0 1 'o', Move 0 2 'x', Move 2 0 'o', Move 1 0 'x', Move 1 2 'o', Move 2 1 'x', Move 0 0 'o', Move 2 2 'x']
      move `shouldBe` Nothing
