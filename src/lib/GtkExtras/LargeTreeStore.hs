-- -*-haskell-*-
--  GIMP Toolkit (GTK) CustomStore TreeModel
--
--  Author : Duncan Coutts, Axel Simon, Sarunas Valaskevicius
--
--  Copyright (C) 2014 Sarunas Valaskevicius
--  Copyright (C) 2005 Duncan Coutts, Axel Simon
--
--  This library is free software; you can redistribute it and/or
--  modify it under the terms of the GNU Lesser General Public
--  License as published by the Free Software Foundation; either
--  version 2.1 of the License, or (at your option) any later version.
--
--  This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY; without even the implied warranty of
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
--  Lesser General Public License for more details.
--
-- |
-- Maintainer  : rakatan@gmail.com
-- Stability   : provisional
-- Portability : portable (depends on GHC)
--
-- Standard model to store hierarchical data.
--
module GtkExtras.LargeTreeStore (
-- * Types
  TreeStore,

-- * Constructors
  treeStoreNew,
  treeStoreNewDND,

-- * Implementation of Interfaces
  treeStoreDefaultDragSourceIface,
  treeStoreDefaultDragDestIface,

-- * Methods
  treeStoreGetValue,
  treeStoreSetValue,

  treeStoreChange,
  treeStoreChangeM,

  treeStoreInsertForest,
  treeStoreInsert,
  treeStoreInsertTree,

  treeStoreRemove,
  treeStoreClear,

  treeStoreGetTree,
  ) where

import Control.Monad                         (liftM, void, when)
import Control.Monad.Trans                   (liftIO)
import Data.Functor                          ((<$>))
import Data.IORef
import Data.Maybe                            (fromJust)
import Data.NestedSet
import Data.Tree
import Foreign.C.Types                       (CInt (..))
import Graphics.UI.Gtk.ModelView.CustomStore
import Graphics.UI.Gtk.ModelView.TreeDrag
import Graphics.UI.Gtk.ModelView.TreeModel
import System.Glib.GObject

-- | A store for hierarchical data.
--
newtype TreeStore a = TreeStore (CustomStore (IORef (Store a)) a)


instance TypedTreeModelClass TreeStore
instance TreeModelClass (TreeStore a)
instance GObjectClass (TreeStore a) where
  toGObject (TreeStore tm) = toGObject tm
  unsafeCastGObject = TreeStore . unsafeCastGObject

data Store a = Store {
    nestedSets :: NestedSets a
}

-- | Create a new list store.
--
-- * The given rose tree determines the initial content and may be the empty
--   list. Each 'Tree' in the forest corresponds to one top-level node.
--
treeStoreNew :: Forest a -> IO (TreeStore a)
treeStoreNew forest = treeStoreNewDND forest
                        (Just treeStoreDefaultDragSourceIface)
                        (Just treeStoreDefaultDragDestIface)

-- | Create a new list store.
--
-- * In addition to 'treeStoreNew', this function takes an two interfaces
--   to implement user-defined drag-and-drop functionality.
--
treeStoreNewDND :: Forest a -- ^ the inital tree stored in this model
  -> Maybe (DragSourceIface TreeStore a) -- ^ an optional interface for drags
  -> Maybe (DragDestIface TreeStore a) -- ^ an optional interface to handle drops
  -> IO (TreeStore a)
treeStoreNewDND forest mDSource mDDest = do
  storeRef <- newIORef Store {
      nestedSets = forestToNestedSets forest
  }
  let withStore f = liftM f $ readIORef storeRef

  customStoreNew storeRef TreeStore TreeModelIface {
    treeModelIfaceGetFlags = return [],

    treeModelIfaceGetIter = \path -> withStore $
      \Store { nestedSets = sets } -> fromPath sets path,

    treeModelIfaceGetPath = \iter -> withStore $
      \Store { nestedSets = sets } -> toPath sets iter,

    treeModelIfaceGetRow  = withStore . getIterValueInStore,

    treeModelIfaceIterNext = \iter -> withStore $
        \Store { nestedSets = sets } ->
            fmap positionToIter $ nestedSetsNextSiblingPosition sets . positionFromIter $ iter,

    treeModelIfaceIterChildren = maybe (return $ Just invalidIter)
        (\iter -> withStore $
            \Store { nestedSets = sets } ->
                fmap positionToIter $ nestedSetsFirstChildPosition sets . positionFromIter $ iter),

    treeModelIfaceIterHasChild = \iter -> withStore $
        \Store { nestedSets = sets } -> not . null . children . fromJust . nestedSetByPath sets . toPath sets $ iter,

    treeModelIfaceIterNChildren = maybe
        (withStore $ \Store { nestedSets = sets } -> length sets)
        (\iter -> withStore $
            \Store { nestedSets = sets } -> length . children . fromJust . nestedSetByPath sets . toPath sets $ iter),

    treeModelIfaceIterNthChild = \mIter idx  -> maybe
        (withStore $ \Store { nestedSets = sets } -> fromPath sets [idx])
        (\iter -> withStore $
            \Store { nestedSets = sets } -> fromPath sets (toPath sets iter ++ [idx]))
        mIter,

    treeModelIfaceIterParent = \iter -> withStore $
        \Store { nestedSets = sets } ->
            fmap positionToIter $ nestedSetsParentPosition sets . positionFromIter $ iter,

    treeModelIfaceRefNode = \_ -> return (),
    treeModelIfaceUnrefNode = \_ -> return ()
   } mDSource mDDest

-- | Default drag functions for
-- 'Graphics.UI.Gtk.ModelView.TreeStore'. These functions allow the rows of
-- the model to serve as drag source. Any row is allowed to be dragged and the
-- data set in the 'SelectionDataM' object is set with 'treeSetRowDragData',
-- i.e. it contains the model and the 'TreePath' to the row.
treeStoreDefaultDragSourceIface :: DragSourceIface TreeStore row
treeStoreDefaultDragSourceIface = DragSourceIface {
    treeDragSourceRowDraggable = \_ _-> return True,
    treeDragSourceDragDataGet = treeSetRowDragData,
    treeDragSourceDragDataDelete = \model dest@(_:_) -> do
            _ <- liftIO $ treeStoreRemove model dest
            return True

  }

-- | Default drop functions for 'Graphics.UI.Gtk.ModelView.TreeStore'. These
--   functions accept a row and insert the row into the new location if it is
--   dragged into a tree view
-- that uses the same model.
treeStoreDefaultDragDestIface :: DragDestIface TreeStore row
treeStoreDefaultDragDestIface = DragDestIface {
    treeDragDestRowDropPossible = \model _ -> do
      mModelPath <- treeGetRowDragData
      case mModelPath of
        Nothing -> return False
        Just (model', _) -> return (toTreeModel model==toTreeModel model'),
    treeDragDestDragDataReceived = \model dest@(_:_) -> do
      mModelPath <- treeGetRowDragData
      case mModelPath of
        Nothing -> return False
        Just (model', source) ->
          if toTreeModel model /= toTreeModel model' then return False
          else liftIO $ do
            row <- treeStoreGetTree model source
            treeStoreInsertTree model (init dest) (last dest) row
            return True
  }

-- | The invalid tree iterator.
--
invalidIter :: TreeIter
invalidIter = TreeIter 0 0 0 0

-- update the stamp of a tree iter
treeIterSetStamp :: TreeIter -> CInt -> TreeIter
treeIterSetStamp (TreeIter _ a b c) s = TreeIter s a b c

positionFromIter :: TreeIter -> Position
positionFromIter (TreeIter _ _ left right) = (fromIntegral left, fromIntegral right)

positionToIter :: Position -> TreeIter
positionToIter = setPositionToIter invalidIter

setPositionToIter :: TreeIter -> Position -> TreeIter
setPositionToIter (TreeIter stamp a _ _) (left, right) = TreeIter stamp a (fromIntegral left) (fromIntegral right)

-- | Convert an iterator into a path.
--
toPath :: NestedSets a -> TreeIter -> TreePath
toPath sets iter = positionToPath sets (positionFromIter iter) 0
    where positionToPath [] _ _ = []
          positionToPath (first : ds) pos nr
              | position first == pos = [nr]
              | isNestedSetsPositionParent (position first) pos = nr:positionToPath (children first) pos 0
              | otherwise = positionToPath ds pos (nr+1)

-- | Try to convert a path into a 'TreeIter'.
--
fromPath :: NestedSets a -> TreePath -> Maybe TreeIter
fromPath sets path = positionToIter <$> positionFromPath
    where positionFromPath = position <$> nestedSetByPath sets path

getIterValueInStore :: TreeIter -> Store a -> a
getIterValueInStore iter (Store{nestedSets = sets}) = content . fromJust . nestedSetByPath sets . toPath sets $ iter

-- | Insert nodes into the store.
--
-- * The given list of nodes is inserted into given parent at @pos@.
--   If the parent existed, the function returns @Just path@ where @path@
--   is the position of the newly inserted elements. If @pos@ is negative
--   or greater or equal to the number of children of the node at @path@,
--   the new nodes are appended to the list.
--
treeStoreInsertForest ::
    TreeStore a -- ^ the store
 -> TreePath    -- ^ @path@ - the position of the parent
 -> Int         -- ^ @pos@ - the index of the new tree
 -> Forest a    -- ^ the list of trees to be inserted
 -> IO ()
treeStoreInsertForest (TreeStore model) path pos nodes = do
    customStoreInvalidateIters model
    (idx, toggle) <- atomicModifyIORef (customStoreGetPrivate model) $
        \Store { nestedSets = sets } ->
        case insertIntoForest (nestedSetsToForest sets) nodes path pos of
            Nothing -> error ("treeStoreInsertForest: path does not exist " ++ show path)
            Just (newForest, idx, toggle) -> (Store { nestedSets = forestToNestedSets newForest }, (idx, toggle))
    Store { nestedSets = sets } <- readIORef (customStoreGetPrivate model)
    let rpath = reverse path
    stamp <- customStoreGetStamp model
    sequence_ [ let p' = reverse p
                    Just iter = fromPath sets p'
                in treeModelRowInserted model p' (treeIterSetStamp iter stamp)
              | (i, node) <- zip [idx..] nodes
              , p <- paths (i : rpath) node ]
    when toggle $ emitRowChildToggledEvent stamp $ fromPath sets path

    where paths :: TreePath -> Tree a -> [TreePath]
          paths path' Node { subForest = ts } =
              path' : concat [ paths (n:path') t | (n, t) <- zip [0..] ts ]
          emitRowChildToggledEvent _ Nothing = return()
          emitRowChildToggledEvent stamp (Just iter) = treeModelRowHasChildToggled model path (treeIterSetStamp iter stamp)

-- | Insert a node into the store.
--
treeStoreInsertTree ::
    TreeStore a -- ^ the store
 -> TreePath    -- ^ @path@ - the position of the parent
 -> Int         -- ^ @pos@ - the index of the new tree
 -> Tree a      -- ^ the value to be inserted
 -> IO ()
treeStoreInsertTree store path pos node =
  treeStoreInsertForest store path pos [node]

-- | Insert a single node into the store.
--
-- * This function inserts a single node without children into the tree.
--   Its arguments are similar to those of 'treeStoreInsert'.
--
treeStoreInsert ::
    TreeStore a -- ^ the store
 -> TreePath    -- ^ @path@ - the position of the parent
 -> Int         -- ^ @pos@ - the index of the new tree
 -> a           -- ^ the value to be inserted
 -> IO ()
treeStoreInsert store path pos node =
  treeStoreInsertForest store path pos [Node node []]

-- | Insert nodes into a forest.
--
-- * If the parent was found, returns the new tree, the child number
--   and a flag denoting if these new nodes were the first children
--   of the parent.
--
insertIntoForest :: Forest a -> Forest a -> TreePath -> Int ->
                    Maybe (Forest a, Int, Bool)
insertIntoForest forest nodes [] pos
  | pos<0 = Just (forest++nodes, length forest, null forest)
  | otherwise = Just (prev++nodes++next, length prev, null forest)
    where (prev, next) = splitAt pos forest
insertIntoForest forest nodes (p:ps) pos = case splitAt p forest of
  (_, []) -> Nothing
  (prev, Node { rootLabel = val,
                subForest = for}:next) ->
    case insertIntoForest for nodes ps pos of
      Nothing -> Nothing
      Just (for', pos', toggle) -> Just (prev++Node { rootLabel = val,
                                                    subForest = for' }:next,
                                         pos', toggle)

-- | Remove a node from the store.
--
-- * The node denoted by the path is removed, along with all its children.
--   The function returns @True@ if the given node was found.
treeStoreRemove :: TreeStore a -> TreePath -> IO Bool
treeStoreRemove (TreeStore model) path = do
  customStoreInvalidateIters model
  (found, toggle) <- atomicModifyIORef (customStoreGetPrivate model) $
    \store -> case deleteFromNestedSets (nestedSets store) path of
        Nothing -> (store, (False, False))
        Just (sets, toggle) -> (store{nestedSets = sets}, (True, toggle))

  when found $ do
    when (toggle && not (null path)) $ do
      Store{nestedSets = sets} <- readIORef (customStoreGetPrivate model)
      let parent = init path
          Just iter = fromPath sets parent
      stamp <- customStoreGetStamp model
      treeModelRowHasChildToggled model parent (treeIterSetStamp iter stamp)
    treeModelRowDeleted model path
  return found


treeStoreClear :: TreeStore a -> IO ()
treeStoreClear (TreeStore model) = do
    customStoreInvalidateIters model
    Store { nestedSets = sets } <- readIORef (customStoreGetPrivate model)
    writeIORef (customStoreGetPrivate model) Store {
        nestedSets = forestToNestedSets []
        }
    let loop (-1) = return ()
        loop n  = treeModelRowDeleted model [n] >> loop (n-1)
    loop (length sets - 1)

-- | Remove a node from a rose tree.
--
-- * Returns the new tree if the node was found. The returned flag is
--   @True@ if deleting the node left the parent without any children.
--
deleteFromNestedSets :: NestedSets a -> TreePath -> Maybe (NestedSets a, Bool)
deleteFromNestedSets _ [] = Nothing
deleteFromNestedSets sets (p:ps) = case splitAt p sets of
    (_, []) -> Nothing
    (prev, node@NestedSetsNode{children = subSets}:next) ->
        if null ps then Just (prev++next, null prev && null next) else
        case deleteFromNestedSets subSets ps of
            Nothing -> Nothing
            Just (subSets', toggle) -> Just (prev++node{children=subSets'}:next, toggle)

-- | Set a node in the store.
--
treeStoreSetValue :: TreeStore a -> TreePath -> a -> IO ()
treeStoreSetValue store path value = void $ treeStoreChangeM store path (\_ -> return value)


-- | Change a node in the store.
--
-- * Returns @True@ if the node was found. For a monadic version, see
--   'treeStoreChangeM'.
treeStoreChange :: TreeStore a -> TreePath -> (a -> a) -> IO Bool
treeStoreChange store path func = treeStoreChangeM store path (return . func)


-- | Change a node in the store.
--
-- * Returns @True@ if the node was found. For a purely functional version, see
--   'treeStoreChange'.
treeStoreChangeM :: TreeStore a -> TreePath -> (a -> IO a) -> IO Bool
treeStoreChangeM (TreeStore model) path act = do
    customStoreInvalidateIters model
    store@Store { nestedSets = sets } <- readIORef (customStoreGetPrivate model)
    (store'@Store {  nestedSets = sets' }, found) <- do
        mRes <- changeNestedSets sets act path
        return $ case mRes of
            Nothing -> (store, False)
            Just sets' -> (Store { nestedSets = sets' }, True)
    when found $ writeIORef (customStoreGetPrivate model) store'
    let Just iter = fromPath sets' path
    stamp <- customStoreGetStamp model
    when found $ treeModelRowChanged model path (treeIterSetStamp iter stamp)
    return found

-- | Change a node in the 'NestedSets'.
changeNestedSets :: NestedSets a -> (a -> IO a) -> TreePath -> IO (Maybe (NestedSets a))
changeNestedSets _ _ [] = return Nothing
changeNestedSets sets act (p:ps) = case splitAt p sets of
    (_, []) -> return Nothing
    (prev, node : next) -> do
        node' <- (if null ps then updateLeaf else updateBranch) node
        return $ fmap (mergeNode prev next) node'
    where updateLeaf node@NestedSetsNode{content = val} = do
              val' <- act val
              return . Just $ node{content = val'}
          updateBranch node@NestedSetsNode{children = subSets} = do
              subSets' <- changeNestedSets subSets act ps
              case subSets' of
                  Nothing -> return Nothing
                  Just subSets'' -> return . Just $ node{children = subSets''}
          mergeNode prev next node = prev ++ node : next


-- | Extract one node from the current model. Fails if the given
--   'TreePath' refers to a non-existent node.
treeStoreGetValue :: TreeStore a -> TreePath -> IO a
treeStoreGetValue (TreeStore model) path = do
    Store { nestedSets = sets } <- readIORef (customStoreGetPrivate model)
    return $ nestedSetValueByPath sets path
    where nestedSetValueByPath sets = content . fromJust . nestedSetByPath sets

treeStoreGetTree :: TreeStore a -> TreePath -> IO (Tree a)
treeStoreGetTree (TreeStore model) path = do
    Store { nestedSets = sets } <- readIORef (customStoreGetPrivate model)
    return $ nestedSubtree . fromJust . nestedSetByPath sets $ path
    where nestedSubtree node = Node (content node) (map nestedSubtree $ children node)

nestedSetByPath :: NestedSets a -> TreePath -> Maybe (NestedSetsNode a)
nestedSetByPath _ [] = Nothing
nestedSetByPath sets (first:rest) = (sets `maybeNth` first) >>= (`nestedSetChildrenByPath` rest)
    where nestedSetChildrenByPath set ([]) = Just set
          nestedSetChildrenByPath set (p:ds) = (children set `maybeNth` p) >>= (`nestedSetChildrenByPath` ds)

maybeNth :: [a] -> Int -> Maybe a
maybeNth [] _ = Nothing
maybeNth (r:_) 0 = Just r
maybeNth (_:rs) n = maybeNth rs (n-1)

