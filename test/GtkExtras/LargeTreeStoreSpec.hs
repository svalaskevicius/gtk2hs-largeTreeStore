module GtkExtras.LargeTreeStoreSpec (main, spec) where

import Test.Hspec
import GtkExtras.LargeTreeStore
import Data.Tree

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "large tree store" $ do
    it "retrieves a value from the tree store" $ do
        treeStore <- treeStoreNew [Node 'a' [Node 'b' [], Node 'c' [Node 'V' []]]]
        value <- treeStoreGetValue treeStore [0, 1, 0]
        value `shouldBe` 'V'

    it "sets a value to the tree store" $ do
        treeStore <- treeStoreNew [Node 'a' [Node 'b' [], Node 'c' [Node 'V' []]]]
        treeStoreSetValue treeStore [0, 1, 0] 'N'
        value <- treeStoreGetValue treeStore [0, 1, 0]
        value `shouldBe` 'N'

    it "inserts forest into given position" $ do
        treeStore <- treeStoreNew [Node 'a' [Node 'b' [], Node 'c' [Node 'V' []]]]
        treeStoreInsertForest treeStore [0, 1] 0 [Node 'x' []]
        value <- treeStoreGetValue treeStore [0, 1, 0]
        value `shouldBe` 'x'

    it "ignores removing an empty path" $ do
        treeStore <- treeStoreNew [Node 'a' [Node 'b' [], Node 'c' [Node 'V' []]]]
        removed <- treeStoreRemove treeStore []
        removed `shouldBe` False

    it "removes a given path" $ do
        treeStore <- treeStoreNew [Node 'a' [Node 'b' [], Node 'c' [Node 'V' []]]]
        removed <- treeStoreRemove treeStore [0, 1, 0]
        removed `shouldBe` True
        value <- treeStoreGetValue treeStore [0, 1, 0]
        (value `shouldBe` '-') `shouldThrow` anyErrorCall


    it "clears the tree store" $ do
        treeStore <- treeStoreNew [Node 'a' [Node 'b' [], Node 'c' [Node 'V' []]]]
        treeStoreClear treeStore
        value <- treeStoreGetValue treeStore [0]
        (value `shouldBe` '-') `shouldThrow` anyErrorCall


