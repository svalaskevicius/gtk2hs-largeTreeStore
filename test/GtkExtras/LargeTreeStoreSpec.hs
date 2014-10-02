module GtkExtras.LargeTreeStoreSpec (main, spec) where

import Test.Hspec
-- import GtkExtras.LargeTreeStore

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "large tree store" $ do
    return()
    {-
    it "can be constructed" $ do
        treeStore <- treeStoreNew []
        _ <- treeStoreGetTree treeStore []
        (1::Int) `shouldBe` 1

-}
