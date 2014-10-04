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
  ) where
import Data.Tree
import System.Glib.GObject
import Graphics.UI.Gtk.ModelView.CustomStore
import Graphics.UI.Gtk.ModelView.TreeModel
import Graphics.UI.Gtk.ModelView.TreeDrag
import Data.IORef
import Control.Monad.Trans ( liftIO )
import Data.NestedSet
import Control.Monad ( when )

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

-- import Foreign.C.Types (CInt(..))
{-
import Data.Bits
import Data.Word (Word)
import Data.Maybe ( fromMaybe, isJust )
import Control.Monad ( liftM, when )
import Control.Exception (assert)
-- import Graphics.UI.Gtk.ModelView.Types
-- import Graphics.UI.Gtk.Types (GObjectClass(..), TreeModelClass)

--------------------------------------------
-- internal model data types
--

-- update the stamp of a tree iter
treeIterSetStamp :: TreeIter -> CInt -> TreeIter
treeIterSetStamp (TreeIter _ a b c) s = (TreeIter s a b c)


-}
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
  let withStore f = readIORef storeRef >>= return . f
{-
      withStoreUpdateCache f = do
        store <- readIORef storeRef
        let (result, cache') = f store
        writeIORef storeRef store { content = cache' }
        return result
-}
  customStoreNew storeRef TreeStore TreeModelIface {
    treeModelIfaceGetFlags = return [],

    treeModelIfaceGetIter = \path -> undefined,

    treeModelIfaceGetPath = \iter -> undefined,

    treeModelIfaceGetRow  = \iter -> undefined,

    treeModelIfaceIterNext = \iter -> undefined,

    treeModelIfaceIterChildren = \mIter -> undefined,

    treeModelIfaceIterHasChild = \iter -> undefined,

    treeModelIfaceIterNChildren = \mIter -> undefined,

    treeModelIfaceIterNthChild = \mIter idx  -> undefined,

    treeModelIfaceIterParent = \iter -> undefined,

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
            liftIO $ treeStoreRemove model dest
            return True

  }

-- | Default drop functions for 'Graphics.UI.Gtk.ModelView.TreeStore'. These
--   functions accept a row and insert the row into the new location if it is
--   dragged into a tree view
-- that uses the same model.
treeStoreDefaultDragDestIface :: DragDestIface TreeStore row
treeStoreDefaultDragDestIface = DragDestIface {
    treeDragDestRowDropPossible = \model dest -> do
      mModelPath <- treeGetRowDragData
      case mModelPath of
        Nothing -> return False
        Just (model', source) -> return (toTreeModel model==toTreeModel model'),
    treeDragDestDragDataReceived = \model dest@(_:_) -> do
      mModelPath <- treeGetRowDragData
      case mModelPath of
        Nothing -> return False
        Just (model', source@(_:_)) ->
          if toTreeModel model/=toTreeModel model' then return False
          else liftIO $ do
--            row <- treeStoreGetTree model source
--            treeStoreInsertTree model (init dest) (last dest) row
            return True
  }

{-
-- | The invalid tree iterator.
--
invalidIter :: TreeIter
invalidIter = TreeIter 0 0 0 0

-- | Convert an iterator into a path.
--
toPath :: Capacity -> TreeIter -> TreePath
toPath d iter = Nothing

-- | Try to convert a path into a 'TreeIter'.
--
fromPath :: Capacity -> TreePath -> Maybe TreeIter
fromPath _ = Nothing


-- | The 'Cache' type synonym is only used iternally. What it represents
--   the stack during a (fictional) lookup operations.
--   The topmost frame is the node
--   for which this lookup was started and the innermost frame (the last
--   element of the list) contains the root of the tree.
--
type Cache a = [(TreeIter, Forest a)]


-- | Create a traversal structure that allows a pre-order traversal in linear
--   time.
--
-- * The returned structure points at the root of the first level which doesn't
--   really exist, but serves to indicate that it is before the very first
--   node.
--
storeToCache :: Forest a -> Cache a
storeToCache [] = []
storeToCache forest = [(invalidIter, [Node root forest])]
  where
  root = error "TreeStore.storeToCache: accessed non-exitent root of tree"

-- | Extract the store from the cache data structure.
cacheToStore :: Cache a -> Forest a
cacheToStore [] = []
cacheToStore cache = case last cache of (_, [Node _ forest]) -> forest

-- | Advance the traversal structure to the given 'TreeIter'.
--
advanceCache :: Capacity -> TreeIter -> Cache a -> Cache a
advanceCache capacity goal [] = []
advanceCache capacity goal cache@((rootIter,_):_) =
  moveToSameLevel 0 capacity
  where
  moveToSameLevel pos [] = cache
  moveToSameLevel pos (d:ds) =
    let
      goalIdx = getBitSlice goal pos d
      curIdx = getBitSlice rootIter pos d
      isNonZero pos d (ti,_) = getBitSlice ti pos d/=0
    in
    if goalIdx==curIdx then moveToSameLevel (pos+d) ds else
    if goalIdx==0 then dropWhile (isNonZero pos d) cache else
    if curIdx==0 then moveToChild pos (d:ds) cache else
    if goalIdx<curIdx then
      moveToChild pos (d:ds) (dropWhile (isNonZero pos d) cache)
    else let
      -- advance the current iterator to coincide with the goal iterator
      -- at this level
      moveWithinLevel pos d ((ti,forest):parents) = let
          diff = fromIntegral (goalIdx-curIdx)
          (dropped, remain) = splitAt diff forest
          advance = length dropped
          ti' = setBitSlice ti pos d (curIdx+fromIntegral advance)
        in
        if advance==diff then moveToChild (pos+d) ds ((ti',remain):parents)
        else (ti',remain):parents -- node not found
    in moveWithinLevel pos d $ case ds of
        [] -> cache
        (d':_) -> dropWhile (isNonZero (pos+d) d') cache

  -- Descend into the topmost forest to find the goal iterator. The position
  -- and the remainding capacitys specify the index in the cache that is zero.
  -- All indices in front of pos coincide with that of the goal iterator.
  moveToChild :: Int -> Capacity -> Cache a -> Cache a
  moveToChild pos [] cache = cache -- we can't set more than the leaf
  moveToChild pos (d:ds) cache@((ti,forest):parents)
    | getBitSlice goal pos d == 0 = cache
    | otherwise = case forest of
      [] -> cache -- impossible request
      Node { subForest = children }:_ ->
        let
          childIdx :: Int
          childIdx = fromIntegral (getBitSlice goal pos d)-1
          (dropped, remain) = splitAt childIdx children
          advanced = length dropped
          ti' = setBitSlice ti pos d (fromIntegral advanced+1)
        in if advanced<childIdx then ((ti',remain):cache) else
           moveToChild (pos+d) ds ((ti',remain):cache)

-- | Advance to the given iterator and return weather this was successful.
--
checkSuccess :: Capacity -> TreeIter -> Cache a -> (Bool, Cache a)
checkSuccess capacity iter cache = case advanceCache capacity iter cache of
    cache'@((cur,sibs):_) -> (cmp cur iter && not (null sibs), cache')
    [] -> (False, [])
  where
  cmp (TreeIter _ a1 b1 c1) (TreeIter _ a2 b2 c2) =
      a1==a2 && b1==b2 && c2==c2
  cache'@((cur,sibs):_) = advanceCache capacity iter cache

-- | Get the leaf index of this iterator.
--
-- * Due to the way we construct the 'TreeIter's, we can check which the last
--   level of an iterator is: The bit sequence of level n is zero if n is
--   greater or equal to the level that the iterator refers to. The returned
--   triple is (pos, leaf, zero) such that pos..pos+leaf denotes the leaf
--   index and pos+leaf..pos+leaf+zero denotes the bit field that is zero.
--
getTreeIterLeaf :: Capacity -> TreeIter -> (Int, Int, Int)
getTreeIterLeaf ds ti = gTIL 0 0 ds
  where
  gTIL pos dCur (dNext:ds)
    | getBitSlice ti (pos+dCur) dNext==0 = (pos,dCur,dNext)
    | otherwise = gTIL (pos+dCur) dNext ds
  gTIL pos d [] = (pos, d, 0)

-- | Move an iterator forwards on the same level.
--
iterNext :: Capacity -> TreeIter -> Cache a -> (Maybe TreeIter, Cache a)
iterNext capacity iter cache = let
    (pos,leaf,child) = getTreeIterLeaf capacity iter
    curIdx = getBitSlice iter pos leaf
    nextIdx = curIdx+1
    nextIter = setBitSlice iter pos leaf nextIdx
  in
  if nextIdx==bit leaf then (Nothing, cache) else
  case checkSuccess capacity nextIter cache of
    (True, cache) -> (Just nextIter, cache)
    (False, cache) -> (Nothing, cache)

-- | Move down to the child of the given iterator.
--
iterNthChild :: Capacity -> Int -> TreeIter -> Cache a  ->
                (Maybe TreeIter, Cache a)
iterNthChild capacity childIdx_ iter cache = let
    (pos,leaf,child) = getTreeIterLeaf capacity iter
    childIdx = fromIntegral childIdx_+1 :: Word
    nextIter = setBitSlice iter (pos+leaf) child childIdx
  in
  if childIdx>=bit child then (Nothing, cache) else
  case checkSuccess capacity nextIter cache of
    (True, cache) -> (Just nextIter, cache)
    (False, cache) -> (Nothing, cache)

-- | Descend to the first child.
--
iterNChildren :: Capacity -> TreeIter -> Cache a -> (Int, Cache a)
iterNChildren capacity iter cache = case checkSuccess capacity iter cache of
  (True, cache@((_,Node { subForest = forest}:_):_)) -> (length forest, cache)
  (_, cache) -> (0, cache)


-- | Ascend to parent.
--
iterParent :: Capacity -> TreeIter -> Maybe TreeIter
iterParent capacity iter = let
    (pos,leaf,child) = getTreeIterLeaf capacity iter
  in if pos==0 then Nothing else
     if getBitSlice iter pos leaf==0 then Nothing else
     Just (setBitSlice iter pos leaf 0)
-}
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
        \store@Store { nestedSets = sets } ->
        case insertIntoForest (nestedSetsToForest sets) nodes path pos of
            Nothing -> error ("treeStoreInsertForest: path does not exist " ++ show path)
            Just (newForest, idx, toggle) -> (Store { nestedSets = forestToNestedSets newForest }, (idx, toggle))
    return ()
{-
    Store { capacity = capacity } <- readIORef (customStoreGetPrivate model)
    let rpath = reverse path
    stamp <- customStoreGetStamp model
    sequence_ [ let p' = reverse p
                    Just iter = fromPath capacity p'
                in treeModelRowInserted model p' (treeIterSetStamp iter stamp)
              | (i, node) <- zip [idx..] nodes
              , p <- paths (i : rpath) node ]
    let Just iter = fromPath capacity path
    when toggle $ treeModelRowHasChildToggled model path
                  (treeIterSetStamp iter stamp)

    where paths :: TreePath -> Tree a -> [TreePath]
          paths path Node { subForest = ts } =
              path : concat [ paths (n:path) t | (n, t) <- zip [0..] ts ]
-}
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
  (prev, []) -> Nothing
  (prev, Node { rootLabel = val,
                subForest = for}:next) ->
    case insertIntoForest for nodes ps pos of
      Nothing -> Nothing
      Just (for, pos, toggle) -> Just (prev++Node { rootLabel = val,
                                                    subForest = for }:next,
                                       pos, toggle)

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
        
    {-
  when found $ do
    when (toggle && not (null path)) $ do
      store <- readIORef (customStoreGetPrivate model)
      let parent = init path
          Just iter = fromPath capacity parent
      treeModelRowHasChildToggled model parent iter
    treeModelRowDeleted model path
    -}
  return found


treeStoreClear :: TreeStore a -> IO ()
treeStoreClear (TreeStore model) = do
    customStoreInvalidateIters model
    -- Store { nestedSets = sets } <- readIORef (customStoreGetPrivate model)
    writeIORef (customStoreGetPrivate model) Store {
        nestedSets = forestToNestedSets []
        }
--    let loop (-1) = return ()
  --      loop   n  = treeModelRowDeleted model [n] >> loop (n-1)
    --loop (length forest - 1)

-- | Remove a node from a rose tree.
--
-- * Returns the new tree if the node was found. The returned flag is
--   @True@ if deleting the node left the parent without any children.
--
deleteFromNestedSets :: NestedSets a -> TreePath -> Maybe (NestedSets a, Bool)
deleteFromNestedSets sets [] = Nothing
deleteFromNestedSets sets (p:ps) = case splitAt p sets of
    (_, []) -> Nothing
    (prev, node@NestedSetsNode{children = subSets}:next) ->
        if null ps then Just (prev++next, null prev && null next) else
        case deleteFromNestedSets subSets ps of
            Nothing -> Nothing
            Just (subSets, toggle) -> Just (prev++node{children=subSets}:next, toggle)

-- | Set a node in the store.
--
treeStoreSetValue :: TreeStore a -> TreePath -> a -> IO ()
treeStoreSetValue store path value = treeStoreChangeM store path (\_ -> return value)
                                  >> return ()


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
    (store'@Store {  nestedSets = sets }, found) <- do
        mRes <- changeNestedSets sets act path
        return $ case mRes of
            Nothing -> (store, False)
            Just sets' -> (Store { nestedSets = sets' }, True)
    when found $ writeIORef (customStoreGetPrivate model) store'
--    let Just iter = fromPath c path
--    stamp <- customStoreGetStamp model
--    when found $ treeModelRowChanged model path (treeIterSetStamp iter stamp)
    return found

-- | Change a node in the 'NestedSets'.
changeNestedSets :: NestedSets a -> (a -> IO a) -> TreePath -> IO (Maybe (NestedSets a))
changeNestedSets sets act [] = return Nothing
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
                  Just subSets -> return . Just $ node{children = subSets}
          mergeNode prev next node = prev ++ node : next


-- | Extract one node from the current model. Fails if the given
--   'TreePath' refers to a non-existent node.
treeStoreGetValue :: TreeStore a -> TreePath -> IO a
treeStoreGetValue (TreeStore model) path = do
    store@Store { nestedSets = sets } <- readIORef (customStoreGetPrivate model)
    return $ nestedSetValueByPath sets path
    where nestedSetValueByPath a b = content $ nestedSetByPath a b
          nestedSetByPath sets [] = undefined
          nestedSetByPath sets (p:ds) = nestedSetChildrenByPath (sets!!p) ds
          nestedSetChildrenByPath set ([]) = set
          nestedSetChildrenByPath set (p:ds) = nestedSetChildrenByPath ((children set)!!p) ds


