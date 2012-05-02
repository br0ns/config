{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}

-- Main inspiration:
-- http://www.haskell.org/haskellwiki/Xmonad/Config_archive/Brent_Yorgey's_darcs_xmonad.hs

import XMonad
import XMonad.StackSet as W
import qualified Data.Map as M
import Data.Maybe (isNothing, isJust, catMaybes)
import Data.List (isPrefixOf, partition, (\\))
import Control.Monad (liftM2, when)

-- Chats a bit on dbus and sets {terminal = gnome-terminal}
import XMonad.Config.Gnome (gnomeConfig)
-- import XMonad.Config.Desktop

----- Hooks
-- Transparent windows
import XMonad.Hooks.FadeInactive (fadeIf, fadeOutLogHook)
import XMonad.Hooks.ManageHelpers (doCenterFloat)


----- Layout
import qualified XMonad.Layout.Tabbed as Tabbed
import XMonad.Layout.PerWorkspace (onWorkspace)
import XMonad.Layout.WorkspaceDir (workspaceDir, changeDir)
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.Simplest
import XMonad.Layout.ResizableTile
import XMonad.Layout.ThreeColumns

----- Actions
-- Window stack
import XMonad.Actions.CycleWindows (cycleRecentWindows)
import XMonad.Actions.CycleWS
  (prevScreen, nextScreen, swapPrevScreen, swapNextScreen)
import XMonad.Actions.GridSelect
-- For different kind of searches
import XMonad.Actions.Submap (submap)
import qualified XMonad.Actions.Search as Search
import XMonad.Actions.WithAll (killAll)
import XMonad.Actions.TopicSpace
  (TopicConfig (..), checkTopicConfig, switchTopic)
import XMonad.Actions.DynamicWorkspaces
  (addWorkspacePrompt, renameWorkspace, removeWorkspace, addWorkspace)
import XMonad.Actions.CopyWindow
  (copyToAll, killAllOtherCopies, wsContainingCopies)

----- Prompt
import XMonad.Prompt (defaultXPConfig, fgColor, bgColor)
import XMonad.Prompt.Input (inputPrompt, (?+))


----- Util
-- "M-C-x" style keybindings
import XMonad.Util.EZConfig (additionalKeysP, removeKeysP)
import XMonad.Util.Scratchpad (scratchpadSpawnActionCustom,
                               scratchpadManageHook,
                               scratchpadFilterOutWorkspace)

myLayout = ResizableTall 1 (3/100) (5/7) [] |||
           ThreeColMid 1 (3/100) (4/7) |||
           Tabbed.tabbedBottom Tabbed.CustomShrink myTabbedTheme

-- Don't show text in tabs.
instance Tabbed.Shrinker Tabbed.CustomShrink where
  shrinkIt _ _ = []

myTabbedTheme =
  Tabbed.defaultTheme
  { Tabbed.inactiveBorderColor = "#000000"
  , Tabbed.inactiveColor       = "#000000"
  , Tabbed.activeColor         = "#BB0000"
  , Tabbed.activeBorderColor   = "#BB0000"
  , Tabbed.urgentBorderColor   = "#FF0000"
  , Tabbed.decoHeight          = 3
  }

myManageHook =
  [ className =? "Do" --> doIgnore
  , className =? "Pidgin" --> doShift "im"
  , className =? "XChat" --> doShift "im"
  , className =? "Bitcoin" --> doShift "bitcoin"
  , title =? "Calendar" --> doShift "organise"
  , title =? "GMail" --> doShift "organise"
  , className =? "Gimp" --> viewShift "gimp"
  , className =? "Sonata" --> doShift "multimedia"
  , className =? "Rhythmbox" --> doShift "multimedia"
  , title =? "Calculator" --> doCenterFloat
  ]
    where
      viewShift = doF . liftM2 (.) W.greedyView W.shift

myTopics =
  [ "web"
  , "im"
  , "organise"
  , "reading"
  , "inkscape"
  , "gimp"
  , "multimedia"
  , "procrastination"
  , "absalon"
  , "pwnies"
  , "idapro"
  , "virtualbox"
  , "download"
    -- Configuration
  , "emacs"
  , "xmonad"
  , "install"
    -- Coding
  , "sml"
  , "haskell"
  , "python"
    -- Projects
  , "mylib"
  , "preml"
  , "hindsight"
  , "treasure-hunt"
  , "iptest"
  , "bitcoin"
    -- Work
  , "ip"
  ]

myShell = "gnome-terminal"
myBrowser = "chromium-browser"
edit s = spawn ("emacs " ++ s)
shell = spawn myShell
browser s = spawn ("chromium-browser " ++ s)
newBrowser s = spawn ("chromium-browser --new-window " ++ s)
appBrowser s = spawn ("chromium-browser --app=\"" ++ s ++ "\"")

myTopicConfig = TopicConfig
  { topicDirs = M.fromList []
  , topicActions =
       M.fromList $
       [ ("web", browser "")
       , ("im", spawn "pidgin" >>
                spawn "xchat")
       , ("organise", appBrowser "http://gmail.com" >>
                      appBrowser "http://calendar.google.com")
       , ("multimedia", spawn "sonata")
       , ("procrastination", newBrowser
                             "fitocracy.com \
                             \facebook.com \
                             \xkcd.com \
                             \smbc-comics.com \
                             \phdcomics.com/comics.php")
       , ("idapro", spawn "idaq")
       , ("virtualbox", spawn "virtualbox")
       , ("reading", spawn "evince")
       , ("emacs", edit "~/.emacs.d/config/bindings.el")
       , ("xmonad", edit "~/config/xmonad.hs" >>
                    newBrowser
                    "http://xmonad.org/xmonad-docs/xmonad-contrib/index.html")
       , ("install", shell)
       , ("mylib", edit "~/code/sml/mylib/notes.org" >>
                   shell)
       , ("preml", edit "~/code/sml/preml/notes.org" >>
                   shell)
       , ("treasure-hunt", edit "~/study/pcs12/treasure-hunt/chal" >>
                           shell)
       , ("hindsight", edit "~/code/hindsight/src/TODO.org" >>
                       shell)
       , ("haskell", newBrowser "www.haskell.org/hoogle/")
       , ("iptest", edit "main.sml")
       , ("bitcoin", edit "mtgox.py newscalper.py" >>
                     -- spawn "bitcoin" >>
                     spawn "gnome-terminal -x python ticker.py" >>
                     shell >>
                     newBrowser "eclipsemc.com mtgox.com \
                                \blockexplorer.com/q/estimate")
       , ("absalon", newBrowser "punkt.ku.dk")
       , ("ip", edit "timer.txt")
       , ("inkscape", spawn "inkscape")
       , ("gimp", spawn "gimp")
       ]
  -- , defaultTopicAction = const $ shell
  , defaultTopicAction = const $ return ()
  , defaultTopic = "web"
  , maxTopicHistory = 10
  }

setWorkspaceDirs layout =
  onWorkspace "organise" (workspaceDir "~/notes"             layout) $
  onWorkspace "pwnies"   (workspaceDir "~/zomg-pwnies"       layout) $
  onWorkspace "download" (workspaceDir "~/downloads"         layout) $
  onWorkspace "mylib"    (workspaceDir "~/code/sml/mylib"    layout) $
  onWorkspace "preml"    (workspaceDir "~/code/sml/preml"    layout) $
  onWorkspace "study"    (workspaceDir "~/study"             layout) $
  onWorkspace "iptest"   (workspaceDir "~/study/ip2011/test" layout) $
  onWorkspace "bitcoin"  (workspaceDir "~/code/python/mtgox" layout) $
  onWorkspace "sml"      (workspaceDir "~/code/sml"          layout) $
  onWorkspace "haskell"  (workspaceDir "~/code/haskell"      layout) $
  onWorkspace "python"   (workspaceDir "~/code/python"       layout) $
  onWorkspace "ip"       (workspaceDir "~/study/ip2011"      layout) $
  workspaceDir "~"                                           layout

isUnfocusedOnCurrentWS :: Query Bool
isUnfocusedOnCurrentWS = do
  w <- ask
  ws <- liftX $ gets windowset
  let thisWS = w `elem` W.index ws
      unfocused = maybe True (w /=) $ W.peek ws
  return $ thisWS && unfocused

br0nsConfig =
  gnomeConfig
       { modMask = mod4Mask
       , manageHook = manageHook gnomeConfig <+>
                      composeAll myManageHook <+>
                      scratchpadManageHook (W.RationalRect 0.2 0.2 0.6 0.6)
       , layoutHook = smartBorders $
                      setWorkspaceDirs myLayout
       , borderWidth = 0
       , focusFollowsMouse = False
       , logHook = fadeOutLogHook $ fadeIf isUnfocusedOnCurrentWS 0.8
       , XMonad.workspaces = myTopics
       }
       `removeKeysP` ["M-q"]
       `additionalKeysP` myKeys

main = do
  checkTopicConfig myTopics myTopicConfig
  xmonad $ br0nsConfig

myKeys =
  -- CycleWindows
  [ ("M-s", cycleRecentWindows [xK_Super_L] xK_s xK_w)
  -- Rebind mod-q
  , ("M-S-<Esc>", spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi")
  -- GSSelect
  , ("M-g", goToSelected myGSConfig)
  -- Workspace navigation
  , ("M-S-z", shiftToSelectedWS True myGSConfig)
  , ("M-z", goToSelectedWS True myGSConfig)
  -- Screen navigation
  , ("M-<Left>", prevScreen)
  , ("M-<Right>", nextScreen)
  , ("M-S-<Left>", swapPrevScreen)
  , ("M-S-<Right>", swapNextScreen)
  -- Window resizing
  , ("M-S-h", sendMessage MirrorExpand)
  , ("M-S-l", sendMessage MirrorShrink)
  -- Dynamic workspaces
  , ("M-S-d", changeDir myXPConfig)
  , ("M-S-n", addWorkspacePrompt myXPConfig)
  , ("M-S-<Backspace>", killAll >> myRemoveWorkspace)
  , ("M-S-r", renameWorkspace myXPConfig)
  , ("M-S-s", newScratchpad)
  -- Search
  , ("M-'", submap . mySearchMap $ myPromptSearch)
  , ("M-C-'", submap . mySearchMap $ mySelectSearch)
  -- Scratchpad
  , ("M-S-<Space>", scratchpadSpawnActionCustom "gnome-terminal --disable-factory --name scratchpad")
  -- Global window
  , ("M-S-g", toggleGlobal)
  ]

-- from XMonad.Actions.Search
mySearchMap method = M.fromList $
        [ ((0, xK_g), method Search.google)
        , ((0, xK_w), method Search.wikipedia)
        , ((0, xK_h), method Search.hoogle)
        , ((shiftMask, xK_h), method Search.hackage)
        , ((0, xK_s), method Search.scholar)
        , ((0, xK_m), method Search.mathworld)
        , ((0, xK_p), method Search.maps)
        , ((0, xK_d), method Search.dictionary)
        , ((0, xK_a), method Search.alpha)
        , ((0, xK_l), method Search.lucky)
        , ((0, xK_i), method Search.images)
        , ((shiftMask, xK_i), method Search.imdb)
        , ((0, xK_y), method Search.youtube)
        ]
          where hackage =
                  Search.searchEngine "hackage" "http://www.google.dk/search?btnI&q=site%3Ahackage.haskell.org+"

-- Prompt search: get input from the user via a prompt, then run the search in
-- `myBrowser` and automatically switch to the "web" workspace
myPromptSearch (Search.SearchEngine _ site)
  = inputPrompt myXPConfig "Search" ?+ Search.search myBrowser site

-- Select search: do a search based on the X selection
mySelectSearch eng = Search.selectSearchBrowser myBrowser eng

-- Switch to the "web" workspace
viewWeb = windows (W.view "web")

-- Remove workspace unless it's a topic
myRemoveWorkspace :: X ()
myRemoveWorkspace = do
  s <- gets windowset
  case s of
    StackSet {current = W.Screen { workspace = Workspace { tag = this } } } ->
      when (this `notElem`myTopics) removeWorkspace

-- Create a new workspace named "scratchpadX"
newScratchpad :: X ()
newScratchpad = withWindowSet $ \w -> do
  let wss = W.workspaces w
      cws = map W.tag $ Prelude.filter
            (\ws -> "scratchpad" `isPrefixOf`
                    W.tag ws && isJust (W.stack ws)) wss
      num = head $ [0..] \\
            catMaybes (map (readMaybe . drop (length "scratchpad")) cws)
      new = "scratchpad" ++ show num
  when (new `notElem` (map W.tag wss)) $ addWorkspace new
  windows $ W.view new
 where readMaybe s = case reads s of
                       [(r,_)] -> Just r
                       _       -> Nothing

-- Toggle 'global' windows
toggleGlobal = do
  ws <- wsContainingCopies
  if (null ws)
    then windows copyToAll
    else killAllOtherCopies

-- Nizzle colours
myXPConfig = defaultXPConfig
  { fgColor = "#a8a3f7"
  -- , bgColor = "#ff3c6d"}
  , bgColor = "#3f3c6d"}

----- Extension on GridSelect
myGSConfig = defaultGSConfig {gs_navigate = navNSearch}
goToSelectedWS :: Bool -> GSConfig WindowSpace -> X ()
goToSelectedWS =
  withSelectedWS $ switchTopic myTopicConfig . W.tag
  -- withSelectedWS $ windows . W.greedyView . W.tag

shiftToSelectedWS :: Bool -> GSConfig WindowSpace -> X ()
shiftToSelectedWS =
  withSelectedWS $ windows . (\ws -> W.greedyView ws . W.shift ws) . W.tag

withSelectedWS :: (WindowSpace -> X ()) -> Bool -> GSConfig WindowSpace -> X ()
withSelectedWS callback inclEmpty conf = do
  mbws <- gridselectWS inclEmpty conf
  case mbws of
    Just ws -> callback ws
    Nothing -> return ()

-- Includes empty window spaces if {True}
gridselectWS :: Bool -> GSConfig WindowSpace -> X (Maybe WindowSpace)
gridselectWS inclEmpty conf =
  withWindowSet $ \ws -> do
    let hid = W.hidden ws
        vis = map W.workspace $ W.visible ws
        all = scratchpadFilterOutWorkspace $ hid ++ vis
        wss = if inclEmpty
              then let (nonEmp, emp) = partition nonEmptyWS all
                   in nonEmp ++ emp
              else Prelude.filter nonEmptyWS all
        ids = map W.tag wss
    gridselect conf $ zip ids wss

nonEmptyWS :: WindowSpace -> Bool
nonEmptyWS = (/= Nothing) . W.stack

instance HasColorizer WindowSpace where
  defaultColorizer ws isFg =
    if nonEmptyWS ws || isFg
    then stringColorizer (W.tag ws) isFg
         -- Empty workspaces get a dusty-sandy-ish colour
    else return ("#CAC3BA", "white")
