module GtkExtras.LargeTreeStoreSpec (main, spec) where

import Test.Hspec
import GtkExtras.LargeTreeStore
import Data.Tree

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "large tree store" $ do
    it "allows to retrieve a value from the tree store" $ do
        treeStore <- treeStoreNew [Node 'a' [Node 'b' [], Node 'c' [Node 'V' []]]]
        value <- treeStoreGetValue treeStore [1, 2, 1]
        value `shouldBe` 'V'
