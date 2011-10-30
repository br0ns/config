{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE TypeSynonymInstances #-}

import XMonad
import XMonad.StackSet as W
import Data.Map (unions, fromList)
import Data.List (partition)
import Control.Monad (liftM2)

-- Chats a bit on dbus and sets {terminal = gnome-terminal}
import XMonad.Config.Gnome
import XMonad.Config.Desktop

----- Hooks
-- Note: use Hooks.EwmhDesktops for gnome-panel integration
import XMonad.Hooks.DynamicLog    -- For dzen
import XMonad.Hooks.FadeInactive  -- Transparent windows, yay
import XMonad.Hooks.UrgencyHook   -- For Ubuntu updates, Pidgin, XChat
import XMonad.Hooks.ManageHelpers -- {doCenterFloat} puts floating windows in
                                  -- the middle of the screen
import XMonad.Hooks.ManageDocks   -- Avoid struts
-- import XMonad.ManageHook


----- Layout
import XMonad.Layout.Tabbed
import XMonad.Layout.Grid
import XMonad.Layout.TwoPane
-- For the "gimp" and "im" workspace
import XMonad.Layout.IM
import XMonad.Layout.Reflect
-- Meta layouts
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.PerWorkspace
import XMonad.Layout.WorkspaceDir
import XMonad.Layout.NoBorders
import XMonad.Layout.ShowWName


----- Actions
import XMonad.Actions.CycleWS
import XMonad.Actions.CycleRecentWS
import XMonad.Actions.CycleWindows
import XMonad.Actions.GridSelect
-- import MyGridSelect
import XMonad.Actions.TopicSpace
import XMonad.Actions.Submap        -- For different kind of searches
import XMonad.Actions.Search
import XMonad.Actions.WithAll       -- {withAll'} can be used to migrate to
                                    -- another workspace
import XMonad.Actions.SpawnOn       -- Send stuff to the right place
import XMonad.Actions.WindowGo      -- Go to window, if already running
                                    -- (eg. XChat, Sonata, etc.)
import XMonad.Actions.UpdatePointer -- Move the pointer with the focus
import XMonad.Actions.TopicSpace
import XMonad.Actions.DynamicWorkspaces


----- Prompt
import XMonad.Prompt
import XMonad.Prompt.Man
import XMonad.Prompt.AppendFile
import XMonad.Prompt.Ssh
import XMonad.Prompt.Workspace
import XMonad.Prompt.Input


----- Util
import XMonad.Util.Run
import XMonad.Util.Loggers
import XMonad.Util.NamedScratchpad  -- This is quite nifty

myLayout = Tall 1 (3/100) (5/7) ||| simpleTabbedBottom

myManageHook =
  [ className =? "Do" --> doIgnore
  , className =? "Pidgin" --> doShift "im"
  , className =? "XChat" --> doShift "im"
  , className =? "Bitcoin" --> doShift "scratchpad"
  , resource =? "gmail" --> doShift "organise"
  , title =? "Calendar" --> doShift "organise"
  , title =? "GMail" --> doShift "organise"
  , className =? "Gimp" --> viewShift "gimp"
  , className =? "Sonata" --> doShift "multimedia"
  , className =? "Rhythmbox" --> doShift "multimedia"
  ]
    where
      viewShift = doF . liftM2 (.) W.greedyView W.shift

myTopics =
  [ "web"
  , "im"
  , "organise"
  , "reading"
  , "study"
  , "graphics"
  , "gimp"
  , "multimedia"
  , "procrastination"
  , "absalon"
  , "pwnies"
  , "pwnies-util"
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
  , "speciale"
  , "iptest"
  , "bitcoin"
    -- Work
  , "ip"
    -- Named scratch pad
  , "scratchpad"
  ]

myTopicConfig = TopicConfig
  { topicDirs = fromList []
  , topicActions =
       fromList $
       [ ("web", spawn "chromium-browser")
       , ("im", spawn "pidgin" >>
                spawn "xchat")
       , ("organise", spawn "gmail" >>
                      spawn "gcal")
       , ("multimedia", spawn "sonata")
       , ("procrastination", spawn $ inNewBrowser
                             "facebook.com \
                             \xkcd.com \
                             \smbc-comics.com \
                             \phdcomics.com/comics.php")
       , ("idapro", spawn "idaq")
       , ("virtualbox", spawn "virtualbox")
       , ("reading", spawn "evince")
       , ("emacs", spawn "emacs ~/.emacs.d/config/bindings.el")
       , ("xmonad", spawn "emacs ~/config/xmonad.hs" >>
                    spawn (inNewBrowser
                    "http://xmonad.org/xmonad-docs/xmonad-contrib/index.html"))
       , ("install", spawn "gnome-terminal")
       , ("mylib", spawn "emacs MyLib.mlb" >>
                   spawn "gnome-terminal")
       , ("preml", spawn "emacs PreML.mlb" >>
                   spawn "gnome-terminal")
       , ("iptest", spawn "emacs main.sml")
       , ("bitcoin", spawn "emacs mtgox.py newscalper.py" >>
                     -- spawn "bitcoin" >>
                     spawn "gnome-terminal -x python ticker.py" >>
                     spawn "gnome-terminal" >>
                     spawn (inNewBrowser "eclipsemc.com mtgox.com \
                                         \blockexplorer.com/q/estimate"))
       , ("absalon", spawn $ inNewBrowser "punkt.ku.dk")
       , ("ip", spawn "gnome-terminal" >>
                spawn "emacs timer.txt")
       ]
  , defaultTopicAction = const $ spawn "gnome-terminal"
  -- , defaultTopicAction = const $ return ()
  }
    where
      inNewBrowser s = "chromium-browser --new-window " ++ s

setWorkspaceDirs layout =
  onWorkspace "organise" (workspaceDir "~/notes" layout) $
  onWorkspace "pwnies" (workspaceDir "~/zomg-pwnies" layout) $
  onWorkspace "pwnies-util" (workspaceDir "~/zomg-pwnies" layout) $
  onWorkspace "download" (workspaceDir "~/downloads" layout) $
  onWorkspace "mylib" (workspaceDir "~/code/sml/mylib" layout) $
  onWorkspace "preml" (workspaceDir "~/code/sml/preml" layout) $
  onWorkspace "speciale" (workspaceDir "~/study/speciale" layout) $
  onWorkspace "study" (workspaceDir "~/study" layout) $
  onWorkspace "iptest" (workspaceDir "~/study/ip2011/test" layout) $
  onWorkspace "bitcoin" (workspaceDir "~/code/python/mtgox" layout) $
  onWorkspace "ip" (workspaceDir "~/study/ip2011" layout) $
  onWorkspace "sml" (workspaceDir "~/code/sml" layout) $
  onWorkspace "haskell" (workspaceDir "~/code/haskell" layout) $
  onWorkspace "python" (workspaceDir "~/code/python" layout) $
  workspaceDir "~" layout

myLogHook dest = dynamicLogWithPP defaultPP { ppOutput = hPutStrLn dest
                                            , ppVisible = wrap "(" ")"
                                            }


br0nsConfig =
  gnomeConfig
       { modMask = mod4Mask -- default is mod1Mask
       , manageHook = manageHook gnomeConfig <+>
                      manageDocks <+>
                      composeAll myManageHook
       , layoutHook = avoidStrutsOn [U] $
                      smartBorders $
                      showWName $
                      setWorkspaceDirs myLayout
       , borderWidth = 0
       , keys = myKeys
       , focusFollowsMouse = False
       , logHook = do fadeInactiveLogHook 0.7
                      -- myLogHook h
       , XMonad.workspaces = myTopics
       }

main = do
  -- h <- spawnPipe myDzen
  checkTopicConfig myTopics myTopicConfig
  xmonad $ br0nsConfig

myKeys conf@(XConfig {modMask = modm}) =
  unions
  [ keys gnomeConfig conf
  -- , planeKeys modm (Lines 3) Finite
  , fromList $ myNewKeys modm
  ]

myNewKeys modm =
  -- CycleWindows
  [ ((modm, xK_s), cycleRecentWindows [xK_Super_L] xK_s xK_w)
  , ((modm, xK_g), goToSelected myGSConfig)
  , ((modm, xK_c), changeDir defaultXPConfig)
  -- Workspace navigation
  , ((modm .|. shiftMask, xK_z), shiftToSelectedWS False myGSConfig)
  , ((modm, xK_z), goToSelectedWS False myGSConfig)
  , ((modm .|. shiftMask, xK_a), shiftToSelectedWS True myGSConfig)
  , ((modm, xK_a), goToSelectedWS True myGSConfig)
  ]


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
        all = hid ++ vis
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