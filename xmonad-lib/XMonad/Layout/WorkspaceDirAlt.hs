{-# LANGUAGE DeriveDataTypeable, FlexibleInstances, MultiParamTypeClasses, TypeSynonymInstances, PatternGuards #-}

-- Custom fixup by Idolf, which added changeDir_

-----------------------------------------------------------------------------
-- |
-- Module      :  XMonad.Layout.WorkspaceDir
-- Copyright   :  (c) 2007  David Roundy <droundy@darcs.net>
-- License     :  BSD3-style (see LICENSE)
--
-- Maintainer  :  none
-- Stability   :  unstable
-- Portability :  unportable
--
-- WorkspaceDir is an extension to set the current directory in a workspace.
--
-- Actually, it sets the current directory in a layout, since there's no way I
-- know of to attach a behavior to a workspace.  This means that any terminals
-- (or other programs) pulled up in that workspace (with that layout) will
-- execute in that working directory.  Sort of handy, I think.
--
-- Note this extension requires the 'directory' package to be installed.
--
-----------------------------------------------------------------------------

module XMonad.Layout.WorkspaceDirAlt (
                                      -- * Usage
                                      -- $usage
                                      workspaceDir,
                                      changeDir,
                                      changeDir_,
                                      WorkspaceDir,
                                      withDir
                                     ) where

import Prelude hiding (catch)
import Control.Exception
import System.Directory ( setCurrentDirectory, getCurrentDirectory )
import Control.Monad ( when )

import XMonad hiding ( focus )
import XMonad.Util.Run ( runProcessWithInput )
import XMonad.Prompt ( XPConfig )
import XMonad.Prompt.Directory ( directoryPrompt )
import XMonad.Layout.LayoutModifier
import qualified XMonad.StackSet as W

econst :: Monad m => a -> IOException -> m a
econst = const . return

-- $usage
-- You can use this module with the following in your @~\/.xmonad\/xmonad.hs@:
--
-- > import XMonad.Layout.WorkspaceDir
--
-- Then edit your @layoutHook@ by adding the Workspace layout modifier
-- to some layout:
--
-- > myLayout = workspaceDir "~" (Tall 1 (3/100) (1/2))  ||| Full ||| etc..
-- > main = xmonad defaultConfig { layoutHook = myLayout }
--
-- For more detailed instructions on editing the layoutHook see:
--
-- "XMonad.Doc.Extending#Editing_the_layout_hook"
--
-- WorkspaceDir provides also a prompt. To use it you need to import
-- "XMonad.Prompt" and add something like this to your key bindings:
--
-- >  , ((modm .|. shiftMask, xK_x     ), changeDir defaultXPConfig)
--
-- For detailed instruction on editing the key binding see:
--
-- "XMonad.Doc.Extending#Editing_key_bindings".

data WorkspaceDirMsg = Chdir String | WithDir (String -> X ()) deriving ( Typeable )
instance Message WorkspaceDirMsg

data WorkspaceDir a = WorkspaceDir String deriving ( Read, Show )

instance LayoutModifier WorkspaceDir Window where
    modifyLayout (WorkspaceDir d) w r = do tc <- gets (W.currentTag . windowset)
                                           when (tc == W.tag w) $ scd d
                                           runLayout w r
    handleMess (WorkspaceDir d) m
        | Just (Chdir wd) <- fromMessage m = do wd' <- cleanDir wd
                                                return $ Just $ WorkspaceDir wd'
        | Just (WithDir f) <- fromMessage m = do f d
                                                 return Nothing
        | otherwise = return Nothing

workspaceDir :: LayoutClass l a => String -> l a
             -> ModifiedLayout WorkspaceDir l a
workspaceDir s = ModifiedLayout (WorkspaceDir s)

cleanDir :: String -> X String
cleanDir x = scd x >> io getCurrentDirectory

scd :: String -> X ()
scd x = do x' <- io (runProcessWithInput "bash" [] ("echo -n " ++ x) `catch` econst x)
           catchIO $ setCurrentDirectory x'

changeDir :: XPConfig -> X ()
changeDir c = directoryPrompt c "Set working directory: " changeDir_

changeDir_ :: String -> X ()
changeDir_ = sendMessage . Chdir

withDir :: (String -> String -> X ()) -> X ()
withDir f = runOnWorkspaces $ \w@(W.Workspace tag l _) -> do
              _ <- handleMessage l . SomeMessage . WithDir $ f tag
              return w
