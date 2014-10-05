module GtkExtras.LargeTreeStoreSpec (main, spec) where

import Test.Hspec
import GtkExtras.LargeTreeStore as LTS
import Data.Tree
import Graphics.UI.Gtk
import Data.IORef

main :: IO ()
main = hspec spec

spec :: Spec
spec = describe "large tree store" $ do
    it "retrieves a value from the tree store" $ do
        treeStore <- LTS.treeStoreNew [Node 'a' [Node 'b' [], Node 'c' [Node 'V' []]]]
        value <- LTS.treeStoreGetValue treeStore [0, 1, 0]
        value `shouldBe` 'V'

    it "sets a value to the tree store" $ do
        treeStore <- LTS.treeStoreNew [Node 'a' [Node 'b' [], Node 'c' [Node 'V' []]]]
        LTS.treeStoreSetValue treeStore [0, 1, 0] 'N'
        value <- LTS.treeStoreGetValue treeStore [0, 1, 0]
        value `shouldBe` 'N'

    it "emits row changed event on setting a value on the tree store" $ do
        treeStore <- LTS.treeStoreNew ([Node 1 [Node 2 [], Node 3 [Node 4 []]]]::Forest Int)
        customStoreSetColumn treeStore (makeColumnIdInt 0) id
        recorder <- recordRowChangedEvents treeStore

        LTS.treeStoreSetValue treeStore [0, 1, 0] 5
        emittedEvents <- recorder
        length emittedEvents `shouldBe` 1
        let (path, iter) = head emittedEvents
        path `shouldBe` [0, 1, 0]
        value <- treeModelGetValue treeStore iter (makeColumnIdInt 0)
        value `shouldBe` 5

    it "inserts forest into given position" $ do
        treeStore <- LTS.treeStoreNew [Node 'a' [Node 'b' [], Node 'c' [Node 'V' []]]]
        LTS.treeStoreInsertForest treeStore [0, 1] 0 [Node 'x' []]
        value <- LTS.treeStoreGetValue treeStore [0, 1, 0]
        value `shouldBe` 'x'

    it "ignores removing an empty path" $ do
        treeStore <- LTS.treeStoreNew [Node 'a' [Node 'b' [], Node 'c' [Node 'V' []]]]
        removed <- LTS.treeStoreRemove treeStore []
        removed `shouldBe` False

    it "removes a given path" $ do
        treeStore <- LTS.treeStoreNew [Node 'a' [Node 'b' [], Node 'c' [Node 'V' []]]]
        removed <- LTS.treeStoreRemove treeStore [0, 1, 0]
        removed `shouldBe` True
        value <- LTS.treeStoreGetValue treeStore [0, 1, 0]
        (value `shouldBe` '-') `shouldThrow` anyErrorCall


    it "clears the tree store" $ do
        treeStore <- LTS.treeStoreNew [Node 'a' [Node 'b' [], Node 'c' [Node 'V' []]]]
        LTS.treeStoreClear treeStore
        value <- LTS.treeStoreGetValue treeStore [0]
        (value `shouldBe` '-') `shouldThrow` anyErrorCall

recordRowChangedEvents :: TreeModelClass tm => tm -> IO (IO [(TreePath, TreeIter)])
recordRowChangedEvents treeStore = do
    recorder <- newIORef []
    _ <- treeStore `on` rowChanged $ \path iter -> do
        prevItems <- readIORef recorder
        writeIORef recorder $ (path, iter):prevItems
    return (readIORef recorder)


