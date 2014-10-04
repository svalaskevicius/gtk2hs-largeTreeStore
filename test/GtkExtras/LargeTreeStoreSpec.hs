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
