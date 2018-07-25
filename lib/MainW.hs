{-# LANGUAGE GADTs               #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE UnicodeSyntax       #-}

module MainW where

import           Control.Monad
import           Control.Monad.Fix
import           Data.List                    (intercalate)
import           Data.Semigroup               ((<>))
import           Data.Map                     (Map)
import qualified Data.Map                     as Map
import           Data.Set                     (Set)
import qualified Data.Set                     as Set
import           Data.Text                    (Text)
import qualified Data.Text                    as T
import           Data.Tree                    ( Tree (Node), rootLabel
                                              , subForest, flatten)

import           Language.Javascript.JSaddle
import           Reflex
import           Reflex.Dom                   hiding (mainWidget)
import           Reflex.Dom.Core              (mainWidget)

mainW ∷ JSM ()
mainW = mainWidget example
-- mainW = mainWidget example2
-- mainW = mainWidget example3
-- mainW = mainWidget example4 

-- Example uses $dyn$ and $switchHold$.
-- Example2 uses $widgetHold$ and $switchDyn$.
-- Example3 take dynamic configuration, but has $dyn$ and $switchHold$ inside.
-- Example4 mimicks switcher but is basicly a combination of holdDyn and 3.

--------------------------------------------------------------------------------


data Conf t = Conf
    { cLabel ∷ Text            -- label is shown
    , cState ∷ Dynamic t Bool  -- coloring is based on state
    , cPath  ∷ Dynamic t [Int] -- unique "path", for the node ev handling
    }

-- hmm
instance Eq (Conf t) where (==) a b = cLabel a == cLabel b

-- Initial tree configuration
confValsI ∷ Reflex t ⇒ Tree (Conf t)
confValsI = Node (Conf "root [0] (adds nodes)" (constDyn False) (constDyn [0]) )
    [ Node (Conf "nd [0,0]" (constDyn False) (constDyn [0,0]) )
        [ Node (Conf "nd [0,0,0]" (constDyn False) (constDyn [0,0,0]) ) []
        , Node (Conf "nd [0,0,1]" (constDyn False) (constDyn [0,0,1]) ) []
        ]
    , Node (Conf "nd [0,1]" (constDyn False) (constDyn [0,1]) )
        [ Node (Conf "nd [0,1,0]" (constDyn False) (constDyn [0,1,0]) ) []
        , Node (Conf "nd [0,1,1]" (constDyn False) (constDyn [0,1,1]) ) []
        ]
    ]

--------------------------------------------------------------------------------

-- for a single node
drawLi ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m )
       ⇒ Conf t → m (Event t [Int])
drawLi c = do
    (el,_) ← elDynAttr' "li" (dA (cState c)) $ text (cLabel c)
    pure $ tag (current (cPath c)) (domEvent Click el)
  where
    dA ∷ Dynamic t Bool → Dynamic t (Map Text Text)
    dA = fmap (\b → "style" =: if b then "color:red" else "color:blue")

-- Output a configured tree and give back an event telling which node
-- was pressed.
mkTree ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m )
       ⇒ Tree (Conf t) → m (Event t [Int])
mkTree tr = do
    evR ∷ Event t [Int] ← drawLi (rootLabel tr)
    evs ∷ [Event t [Int]] ← el "ul" $ mapM mkTree (subForest tr)
    pure $ leftmost $ evR:evs

--------------------------------------------------------------------------------

-- With ghc (webgtk/warp with ghci): after node addition, the traces show
-- each thing twice (but not before). Ghcjs doesn't do that.
example ∷ forall t m. ( Reflex t, DomBuilder t m, PostBuild t m
                      , MonadFix m, MonadHold t m )
        ⇒ m ()
example = mdo
    dTr ∷ Dynamic t (Tree (Conf t)) ← holdDyn confValsI $ leftmost [evN,eTr]
    ev  ∷ Event t (Event t [Int])   ← dyn $ ffor dTr mkTree
    ev2 ∷ Event t [Int]             ← switchHold never ev
    let evMore = traceEvent "evMore" $ ffilter (== [0]) ev2 -- add node
        evAnot = traceEvent "evAnot" $ ffilter (/= [0]) ev2 -- change state
        -- Event for changing a state of a node:
        eTr ∷ Event t (Tree (Conf t)) =
            attachWith (\tr pth →
                         fmap (\l →
                              let dNs = fmap
                                     (\(p,b) → if p == pth then not b else b
                                     ) $ zipDyn (cPath l) (cState l)
                               in l { cState = dNs }
                              ) tr
                       ) (current dTr) evAnot
    -- Used to give a unique name:
    dInt ∷ Dynamic t Int ← foldDyn (+) 1 $ mergeWith (+) [1 <$ evMore]
    -- If pressed on a node with "path [0]", then add a new root.
    let -- Construct a new "path" for the new node. And then build a
        -- new tree configuration.
        -- dNew2 ∷ Dynamic t (Tree (Conf t)) =
        dNew ∷ Dynamic t (Tree (Conf t)) = -- Extra ev, doesn't initialize states
            fmap (\(tr,i) →
                  Node (Conf ("A Text " <> (T.pack . show) i)
                             (constDyn False)
                             (constDyn [1,i] )
                       ) [tr]
                 ) $ zipDyn dTr dInt
    -- dNew ← holdUniqDyn dNew2 -- No extra ev but initilizes all states.
    -- Why does it initialize states?
    -- (holdUniqDyn behaves similarly with ghcjs and ghc.)
    --
    -- Event when adding a new node: this gives the new configuration.
    let evN ∷ Event t (Tree (Conf t)) = traceEventWith
            (\tr →  "evN:" ++ intercalate ","
                (fmap (T.unpack . cLabel) (flatten tr) ))
            $ tag (current dNew) evMore
    pure ()

--------------------------------------------------------------------------------

example2 ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m
                      , MonadFix m, MonadHold t m )
         ⇒ m ()
example2 = mdo
    dEv ∷ Dynamic t (Event t [Int]) ← widgetHold (mkTree confValsI)
            $ leftmost [ mkTree <$> eTr
                       , mkTree <$> evN
                       ]
    let ev2 ∷ Event t [Int] = switchDyn dEv
    let evMore = traceEvent "evMore" $ ffilter (== [0]) ev2
        evAnot = traceEvent "evAnot" $ ffilter (/= [0]) ev2
        eTr ∷ Event t (Tree (Conf t)) =
            attachWith (\trc pth →
                         fmap (\l →
                              let dNs = fmap
                                     (\(p,b) → if p == pth then not b else b
                                     ) $ zipDyn (cPath l) (cState l)
                               in l { cState = dNs }
                              ) trc
                       ) (current dTr) evAnot
    dInt ∷ Dynamic t Int ← foldDyn (+) 1 $ mergeWith (+) [1 <$ evMore]
    dTr ∷ Dynamic t (Tree (Conf t)) ← holdDyn confValsI $ leftmost [eTr,evN]
    let dNew ∷ Dynamic t (Tree (Conf t)) =
            fmap (\(tr,i) →
                  Node (Conf ("A Text " <> (T.pack . show) i)
                             (constDyn False)
                             (constDyn [1,i] )
                       ) [tr]
                 ) $ zipDyn dTr dInt
        evN ∷ Event t (Tree (Conf t)) = traceEventWith
            (\tr →  "evN:" ++ intercalate ","
                (fmap (T.unpack . cLabel) (flatten tr) ))
            $ tag (current dNew) evMore
    pure ()

--------------------------------------------------------------------------------

-- For a single node. Eats dynamic configuration.
drawLiDyn ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m)
       ⇒ Dynamic t (Conf t) → m (Event t [Int])
drawLiDyn dc = do
    let dB = join $ cState <$> dc
        dP = join $ cPath <$> dc
    (el,_) ← elDynAttr' "li" (dA dB) $ dynText (cLabel <$> dc)
    pure $ tag (current dP) (domEvent Click el)
  where
    dA ∷ Dynamic t Bool → Dynamic t (Map Text Text)
    dA = fmap (\b → "style" =: if b then "color:red" else "color:blue")

-- Output a configured tree and give back an event telling which node
-- was pressed. Eats dynamic configuration but uses dyn and mkTree inside.
mkTreeDyn ∷ forall t m. (Reflex t, DomBuilder t m, PostBuild t m, MonadHold t m )
       ⇒ Dynamic t (Tree (Conf t)) → m (Event t [Int])
mkTreeDyn dtr = do
    let drl ∷ Dynamic t (Conf t) = fmap rootLabel dtr
        dsf ∷ Dynamic t [Tree (Conf t)] = fmap subForest dtr
        -- sfDs ∷ [Dynamic t (Tree (Conf t))] = sequence dsf
    evR ∷ Event t [Int] ← drawLiDyn drl
    evEvs ∷ Event t [Event t [Int]] ← el "ul" $
             dyn $ ffor dsf $ \sf → mapM mkTree sf
    let evEv = fmap leftmost evEvs
    evSf ← switchHold never evEv
    pure $ leftmost [evR, evSf]


example3 ∷ forall t m. ( Reflex t, DomBuilder t m, PostBuild t m
                      , MonadFix m, MonadHold t m )
        ⇒ m ()
example3 = mdo
    dTr ∷ Dynamic t (Tree (Conf t)) ← holdDyn confValsI $ leftmost [evN,eTr]
    ev  ∷ Event t [Int] ← mkTreeDyn dTr
    let evMore = traceEvent "evMore" $ ffilter (== [0]) ev -- add node
        evAnot = traceEvent "evAnot" $ ffilter (/= [0]) ev -- change state
        -- Event for changing a state of a node:
        eTr ∷ Event t (Tree (Conf t)) =
            attachWith (\tr pth →
                         fmap (\l →
                              let dNs = fmap
                                     (\(p,b) → if p == pth then not b else b
                                     ) $ zipDyn (cPath l) (cState l)
                               in l { cState = dNs }
                              ) tr
                       ) (current dTr) evAnot
    -- Used to give a unique name:
    dInt ∷ Dynamic t Int ← foldDyn (+) 1 $ mergeWith (+) [1 <$ evMore]
    -- If pressed on a node with "path [0]", then add a new root.
    let -- Construct a new "path" for the new node. And then build a
        -- new tree configuration.
        dNew ∷ Dynamic t (Tree (Conf t)) =
            fmap (\(tr,i) →
                  Node (Conf ("A Text " <> (T.pack . show) i)
                             (constDyn False)
                             (constDyn [1,i] )
                       ) [tr]
                 ) $ zipDyn dTr dInt
        -- Event when adding a new node: this gives the new configuration.
        evN ∷ Event t (Tree (Conf t)) = traceEventWith
            (\tr →  "evN:" ++ intercalate ","
                (fmap (T.unpack . cLabel) (flatten tr) ))
            $ tag (current dNew) evMore
    pure ()


--------------------------------------------------------------------------------

example4 ∷ forall t m. ( Reflex t, DomBuilder t m, PostBuild t m
                      , MonadFix m, MonadHold t m )
        ⇒ m ()
example4 = mdo
    let dCF = constDyn confValsI
    dTr ∷ Dynamic t (Tree (Conf t)) ← switcherDyn dCF $ leftmost [evN,eTr]
    -- The following dyn and switchHold would lead to internal errors:
    -- ev  ∷ Event t (Event t [Int])   ← dyn $ ffor dTr mkTree
    -- ev2 ∷ Event t [Int]             ← switchHold never ev
    ev2 ∷ Event t [Int] ← mkTreeDyn dTr
    let evMore = traceEvent "evMore" $ ffilter (== [0]) ev2 -- add node
        evAnot = traceEvent "evAnot" $ ffilter (/= [0]) ev2 -- change state
    dAnot ← holdDyn [] evAnot
    let dTr2 = fmap (\(tr,pth) →
                         fmap (\l →
                              let dNs = fmap
                                     (\(p,b) → if p == pth then not b else b
                                     ) $ zipDyn (cPath l) (cState l)
                               in l { cState = dNs }
                              ) tr
                 ) $ zipDyn dTr dAnot
        -- Event for changing a state of a node:
        eTr ∷ Event t (Dynamic t (Tree (Conf t))) = dTr2 <$ evAnot
    -- Used to give a unique name:
    dInt ∷ Dynamic t Int ← foldDyn (+) 1 $ mergeWith (+) [1 <$ evMore]
    -- If pressed on a node with "path [0]", then add a new root.
    let -- Construct a new "path" for the new node. And then build a
        -- new tree configuration.
        dNew ∷ Dynamic t (Tree (Conf t)) =
            fmap (\(tr,i) →
                  Node (Conf ("A Text " <> (T.pack . show) i)
                             (constDyn False)
                             (constDyn [1,i] )
                       ) [tr]
                 ) $ zipDyn dTr dInt
        -- Event when adding a new node: this gives the new configuration.
        evN ∷ Event t (Dynamic t (Tree (Conf t))) = dNew <$ evMore
    pure ()

-- | This is basically a holdDyn, made along the lines of "switcher"
switcherDyn ∷ forall a t m. (Reflex t, MonadHold t m)
           ⇒ Dynamic t a → Event t (Dynamic t a) → m (Dynamic t a)
switcherDyn d ed = pure . join =<< holdDyn d ed

--------------------------------------------------------------------------------



