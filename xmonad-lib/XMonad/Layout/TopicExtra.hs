module XMonad.Layout.TopicExtra
      ( isUnfocusedOnCurrentWS
      , goToSelectedWS
      , shiftToSelectedWS
      , addWorkspaceMoveWindow
      , addWorkspaceMoveWindowPrompt
      , toggleGlobal
      , nonEmptyWS
      , deleteUnimportant
      , onRescreen
      ) where

import XMonad
import XMonad.Actions.DynamicWorkspaces(addHiddenWorkspace)
import XMonad.Actions.GridSelect(GSConfig, gridselect)
import XMonad.Actions.TopicSpace(TopicConfig, switchTopic)
import XMonad.Util.Scratchpad (scratchpadFilterOutWorkspace)
import XMonad.Prompt(XPConfig, mkXPrompt)
import XMonad.Prompt.Workspace (Wor(Wor))
import XMonad.Actions.CopyWindow
  (copyToAll, killAllOtherCopies, wsContainingCopies)
import qualified XMonad.StackSet as W
import Data.List(partition)
import Data.Maybe(isNothing)
import Data.Monoid(All(..))
import Control.Monad(unless)


-- Toggle 'global' windows
toggleGlobal :: X ()
toggleGlobal = do
  ws <- wsContainingCopies
  if null ws
    then windows copyToAll
    else killAllOtherCopies

addWorkspaceMoveWindow :: String -> X ()
addWorkspaceMoveWindow newtag =
  do addHiddenWorkspace newtag
     windows (W.greedyView newtag . W.shift newtag)

deleteUnimportant :: (String -> Bool) -> ([String] -> X ()) -> Event -> X All
deleteUnimportant unimportant callback (DestroyWindowEvent {}) = do
                          xstate <- get
                          let ws = windowset xstate
                              hidden = W.hidden ws
                              (dead, alive) = partition (\w -> (unimportant . W.tag) w && isNothing (W.stack w)) hidden
                          unless (null dead) $ callback . map W.tag $ dead
                          put $ xstate { windowset = ws { W.hidden = alive } }
                          return (All True)
deleteUnimportant _ _ _ = return (All True)

-- | Prompt for the name of a new workspace, add it if it does not
--   already exist, and switch to it.
addWorkspaceMoveWindowPrompt :: XPConfig -> X ()
addWorkspaceMoveWindowPrompt conf =
  mkXPrompt (Wor "New workspace name: ") conf (const (return [])) addWorkspaceMoveWindow

goToSelectedWS :: TopicConfig -> Bool -> GSConfig WindowSpace -> X ()
goToSelectedWS topicConfig =
  withSelectedWS $ switchTopic topicConfig . W.tag

shiftToSelectedWS :: Bool -> GSConfig WindowSpace -> X ()
shiftToSelectedWS =
  withSelectedWS $ windows . (\ws -> W.greedyView ws . W.shift ws) . W.tag

withSelectedWS :: (WindowSpace -> X ()) -> Bool -> GSConfig WindowSpace -> X ()
withSelectedWS callback inclEmpty conf = do
  mbws <- gridselectWS inclEmpty conf
  case mbws of
    Just ws -> callback ws
    Nothing -> return ()

isUnfocusedOnCurrentWS :: Query Bool
isUnfocusedOnCurrentWS = do
  w <- ask
  ws <- liftX $ gets windowset
  let thisWS = w `elem` W.index ws
      unfocused = maybe True (w /=) $ W.peek ws
  return $ thisWS && unfocused

-- Includes empty window spaces if {True}
gridselectWS :: Bool -> GSConfig WindowSpace -> X (Maybe WindowSpace)
gridselectWS inclEmpty conf =
  withWindowSet $ \ws -> do
    let hid = W.hidden ws
        vis = map W.workspace $ W.visible ws
        w_all = scratchpadFilterOutWorkspace $ hid ++ vis
        wss = if inclEmpty
              then let (nonEmp, emp) = partition nonEmptyWS w_all
                   in nonEmp ++ emp
              else Prelude.filter nonEmptyWS w_all
        ids = map W.tag wss
    gridselect conf $ zip ids wss

nonEmptyWS :: WindowSpace -> Bool
nonEmptyWS = (/= Nothing) . W.stack


onRescreen ::  X () -> Event -> X All
onRescreen u (ConfigureEvent {ev_window = w}) =
    whenX (isRoot w) u >> return (All False)

onRescreen _ _ = return (All True)
