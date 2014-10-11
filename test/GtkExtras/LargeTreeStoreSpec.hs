module GtkExtras.LargeTreeStoreSpec (main, spec) where

import Control.Monad            (zipWithM_)
import Data.IORef
import Data.Maybe               (isNothing)
import Data.Tree
import Graphics.UI.Gtk
import GtkExtras.LargeTreeStore as LTS
import Test.Hspec

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
        treeStore <- numberedTreeStore
        recorder <- recordRowChangedEvents treeStore
        LTS.treeStoreSetValue treeStore [0, 1, 0] 5
        emittedEvents <- recorder
        pathIterEventsShouldBe treeStore emittedEvents [([0, 1, 0], 5)]

    it "inserts forest into given position" $ do
        treeStore <- LTS.treeStoreNew [Node 'a' [Node 'b' [], Node 'c' [Node 'V' []]]]
        LTS.treeStoreInsertForest treeStore [0, 1] 0 [Node 'x' []]
        value <- LTS.treeStoreGetValue treeStore [0, 1, 0]
        value `shouldBe` 'x'

    it "notifies about inserted rows when inserting a forest" $ do
        treeStore <- numberedTreeStore
        recorder <- recordRowInsertedEvents treeStore
        LTS.treeStoreInsertForest treeStore [0, 1] 0 [Node 99 [Node 100 []], Node 101 []]
        emittedEvents <- recorder
        pathIterEventsShouldBe treeStore emittedEvents [([0, 1, 1], 101), ([0, 1, 0, 0], 100), ([0, 1, 0], 99)]

    it "notifies about toggled child on inserting the first child" $ do
        treeStore <- numberedTreeStore
        recorder <- recordChildToggledEvents treeStore
        LTS.treeStoreInsertForest treeStore [0, 0] 0 [Node 99 [Node 100 []], Node 101 []]
        emittedEvents <- recorder
        pathIterEventsShouldBe treeStore emittedEvents [([0, 0], 2)]

    it "does not notify about toggled child on inserting a subsequent child" $ do
        treeStore <- numberedTreeStore
        recorder <- recordChildToggledEvents treeStore
        LTS.treeStoreInsertForest treeStore [0, 1] 0 [Node 99 [Node 100 []], Node 101 []]
        emittedEvents <- recorder
        pathIterEventsShouldBe treeStore emittedEvents []

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


    it "notifies about toggled child on removing the last child" $ do
        treeStore <- numberedTreeStore
        recorder <- recordChildToggledEvents treeStore
        _ <- LTS.treeStoreRemove treeStore [0, 1, 0]
        emittedEvents <- recorder
        pathIterEventsShouldBe treeStore emittedEvents [([0, 1], 3)]

    it "notifies about a removed row" $ do
        treeStore <- numberedTreeStore
        recorder <- recordRowDeletedEvents treeStore
        _ <- LTS.treeStoreRemove treeStore [0, 1, 0]
        emittedEvents <- recorder
        emittedEvents `pathEventsShouldBe` [[0, 1, 0]]


    it "does not notify about toggled child on removing not the last child" $ do
        treeStore <- numberedTreeStore
        recorder <- recordChildToggledEvents treeStore
        _ <- LTS.treeStoreRemove treeStore [0, 0]
        emittedEvents <- recorder
        pathIterEventsShouldBe treeStore emittedEvents []

    it "clears the tree store" $ do
        treeStore <- LTS.treeStoreNew [Node 'a' [Node 'b' [], Node 'c' [Node 'V' []]]]
        LTS.treeStoreClear treeStore
        value <- LTS.treeStoreGetValue treeStore [0]
        (value `shouldBe` '-') `shouldThrow` anyErrorCall

    it "notifies about a removed top level rows when clearing store" $ do
        treeStore <- numberedTreeStore
        recorder <- recordRowDeletedEvents treeStore
        _ <- LTS.treeStoreClear treeStore
        emittedEvents <- recorder
        emittedEvents `pathEventsShouldBe` [[0], [1]]

    it "can extract a subtree" $ do
        treeStore <- numberedTreeStore
        tree <- LTS.treeStoreGetTree treeStore [0, 1]
        tree `shouldBe` Node 3 [Node 4 []]

    it "can be retrieve first value" $ do
        treeStore <- numberedTreeStore
        (Just iter) <- treeModelGetIterFirst treeStore
        value <- treeModelGetValue treeStore iter (makeColumnIdInt 0)
        value `shouldBe` 1

    it "can be retrieve iter path" $ do
        treeStore <- numberedTreeStore
        (Just iter) <- treeModelGetIter treeStore [0, 1, 0]
        path <- treeModelGetPath treeStore iter
        path `shouldBe` [0, 1, 0]

    it "can be iterated to next sibling" $ do
        treeStore <- numberedTreeStore
        (Just iter) <- treeModelGetIterFirst treeStore
        (Just sibling) <- treeModelIterNext treeStore iter
        value <- treeModelGetValue treeStore sibling (makeColumnIdInt 0)
        value `shouldBe` 5

    it "can be iterated to first child" $ do
        treeStore <- numberedTreeStore
        (Just iter) <- treeModelGetIterFirst treeStore
        (Just child) <- treeModelIterChildren treeStore iter
        value <- treeModelGetValue treeStore child (makeColumnIdInt 0)
        value `shouldBe` 2

    it "can check if iter has child when it doesn't" $ do
        treeStore <- numberedTreeStore
        (Just iter) <- treeModelGetIter treeStore [0, 1, 0]
        hasChild <- treeModelIterHasChild treeStore iter
        hasChild `shouldBe` False

    it "can check if iter has child when it does" $ do
        treeStore <- numberedTreeStore
        (Just iter) <- treeModelGetIter treeStore [0, 1]
        hasChild <- treeModelIterHasChild treeStore iter
        hasChild `shouldBe` True

    it "can check how many children does an iter have" $ do
        treeStore <- numberedTreeStore
        (Just iter) <- treeModelGetIter treeStore [0, 1]
        nChildren <- treeModelIterNChildren treeStore $ Just iter
        nChildren `shouldBe` 1

    it "can check how many children there are at the top level" $ do
        treeStore <- LTS.treeStoreNew ([Node 1 [Node 2 [], Node 3 [Node 4 []]], Node 5 []]::Forest Int)
        nChildren <- treeModelIterNChildren treeStore Nothing
        nChildren `shouldBe` 2

    it "returns nth child" $ do
        treeStore <- LTS.treeStoreNew ([Node 1 [Node 2 [], Node 3 [Node 4 [], Node 6 []]], Node 5 []]::Forest Int)
        (Just iter) <- treeModelGetIter treeStore [0, 1]
        (Just child) <- treeModelIterNthChild treeStore (Just iter) 1
        path <- treeModelGetPath treeStore child
        path `shouldBe` [0, 1, 1]

    it "returns Nothing on empty path" $ do
        treeStore <- LTS.treeStoreNew ([Node 1 [Node 2 [], Node 3 [Node 4 [], Node 6 []]], Node 5 []]::Forest Int)
        mIter <- treeModelGetIter treeStore []
        isNothing mIter `shouldBe` True

    it "returns Nothing on out-of-band path" $ do
        treeStore <- LTS.treeStoreNew ([Node 1 [Node 2 [], Node 3 [Node 4 [], Node 6 []]], Node 5 []]::Forest Int)
        mIter <- treeModelGetIter treeStore [99, 10111]
        isNothing mIter `shouldBe` True

    it "returns nth root" $ do
        treeStore <- LTS.treeStoreNew ([Node 1 [Node 2 [], Node 3 [Node 4 [], Node 6 []]], Node 5 []]::Forest Int)
        (Just child) <- treeModelIterNthChild treeStore Nothing 1
        path <- treeModelGetPath treeStore child
        path `shouldBe` [1]

    it "returns parent iter as Nothing if there is no parent" $ do
        treeStore <- LTS.treeStoreNew ([Node 1 [Node 2 [], Node 3 [Node 4 [], Node 6 []]], Node 5 []]::Forest Int)
        (Just iter) <- treeModelGetIter treeStore [1]
        mParent <- treeModelIterParent treeStore iter
        isNothing mParent `shouldBe` True

    it "returns parent iter if there is a parent node" $ do
        treeStore <- LTS.treeStoreNew ([Node 1 [Node 2 [], Node 3 [Node 4 [], Node 6 []]], Node 5 []]::Forest Int)
        (Just iter) <- treeModelGetIter treeStore [0, 1]
        (Just parent) <- treeModelIterParent treeStore iter
        path <- treeModelGetPath treeStore parent
        path `shouldBe` [0]



recordRowChangedEvents :: TreeModelClass tm => tm -> IO (IO [(TreePath, TreeIter)])
recordRowChangedEvents = recordPathIterEvents rowChanged

recordChildToggledEvents :: TreeModelClass tm => tm -> IO (IO [(TreePath, TreeIter)])
recordChildToggledEvents = recordPathIterEvents rowHasChildToggled

recordRowInsertedEvents :: TreeModelClass tm => tm -> IO (IO [(TreePath, TreeIter)])
recordRowInsertedEvents = recordPathIterEvents rowInserted

recordPathIterEvents :: TreeModelClass tm => Signal tm (TreePath -> TreeIter -> IO ()) -> tm -> IO (IO [(TreePath, TreeIter)])
recordPathIterEvents event treeStore = do
    recorder <- newIORef []
    _ <- treeStore `on` event $ \path iter -> do
        prevItems <- readIORef recorder
        writeIORef recorder $ (path, iter):prevItems
    return (readIORef recorder)

recordRowDeletedEvents :: TreeModelClass tm => tm -> IO (IO [TreePath])
recordRowDeletedEvents treeStore = do
    recorder <- newIORef []
    _ <- treeStore `on` rowDeleted $ \path  -> do
        prevItems <- readIORef recorder
        writeIORef recorder $ path:prevItems
    return (readIORef recorder)

pathIterEventsShouldBe :: (TreeModelClass self, Show a, Eq a) =>
                          self -> [(a, TreeIter)] -> [(a, Int)] -> IO ()
pathIterEventsShouldBe treeStore emittedEvents expectedValues = do
    length emittedEvents `shouldBe` length expectedValues
    zipWithM_ compareEvent emittedEvents expectedValues
    where compareEvent (path, iter) (path', value') = do
            path `shouldBe` path'
            value <- treeModelGetValue treeStore iter (makeColumnIdInt 0)
            value `shouldBe` value'

pathEventsShouldBe :: (Show a, Eq a) => [a] -> [a] -> IO ()
pathEventsShouldBe emittedEvents expectedValues = do
    length emittedEvents `shouldBe` length expectedValues
    zipWithM_ compareEvent emittedEvents expectedValues
    where compareEvent path path' = path `shouldBe` path'

numberedTreeStore :: IO (LTS.TreeStore Int)
numberedTreeStore = do
    treeStore <- LTS.treeStoreNew ([Node 1 [Node 2 [], Node 3 [Node 4 []]], Node 5 []]::Forest Int)
    customStoreSetColumn treeStore (makeColumnIdInt 0) id
    return treeStore
