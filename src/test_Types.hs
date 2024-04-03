import Test.Hspec
import Types

main :: IO ()
main = hspec $ do
  describe "Show instance for TimeSlot" $ do
    it "returns the expected string representation" $ do
      let timeSlot = TimeSlot 10 30
      show timeSlot `shouldBe` "TimeSlot {hour = 10, minute = 30}"