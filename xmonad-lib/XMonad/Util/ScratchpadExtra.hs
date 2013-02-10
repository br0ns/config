module XMonad.Util.ScratchpadExtra where

import XMonad
import XMonad.StackSet as W
import XMonad.Actions.DynamicWorkspaces (addWorkspace)

import Control.Monad (when)
import Data.Maybe (isJust, mapMaybe)
import Data.List (isPrefixOf, (\\))

-- Create a new workspace named "scratchpadX"
newScratchpad :: X ()
newScratchpad = withWindowSet $ \w -> do
  let wss = W.workspaces w
      cws = map W.tag $ Prelude.filter
            (\ws -> sprefix `isPrefixOf`
                    W.tag ws && isJust (W.stack ws)) wss
      num = head $ ([0..] :: [Int]) \\
            mapMaybe (readMaybe . drop (length sprefix)) cws
      name = sprefix ++ show num
  when (name `notElem` map W.tag wss) $ addWorkspace name
  windows $ W.view name
 where sprefix = "scratchpad-"
       readMaybe s = case reads s of
                       [(r,_)] -> Just r
                       _       -> Nothing
