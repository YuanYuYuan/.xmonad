-- { Imports } {{{
-- import Data.Char
-- import Data.Monoid
-- import XMonad.Layout.Fullscreen
import XMonad.Layout.Gaps
import Data.Ratio ((%))
import Data.List
-- import XMonad.Layout.MouseResizableTile
-- import XMonad.Util.WorkspaceCompare
-- import qualified Data.Map        as M
import XMonad.Actions.WindowMenu
import XMonad.Actions.UpdatePointer
import XMonad.Actions.EasyMotion (selectWindow)
import XMonad.Actions.FloatKeys
import XMonad.Hooks.FadeWindows
import XMonad.Layout.TrackFloating
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.InsertPosition
import Control.Monad ( liftM2 )
import Graphics.X11.ExtraTypes
import System.Exit
import XMonad hiding ( (|||) )
import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import XMonad.Hooks.UrgencyHook
import XMonad.Actions.GroupNavigation
  ( historyHook
  , nextMatch
  , Direction ( History )
  )
import XMonad.Actions.MouseResize
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import XMonad.Layout.LayoutCombinators
import XMonad.Layout.LayoutModifier
import XMonad.Layout.NoBorders (smartBorders)
import XMonad.Layout.MultiToggle
import XMonad.Layout.MultiToggle.Instances
import XMonad.Layout.Simplest
import XMonad.Layout.SubLayouts
import XMonad.Layout.WindowNavigation
import XMonad.Layout.Renamed (renamed, Rename(Replace))
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.WindowArranger
import XMonad.Layout.Grid
import XMonad.Layout.ResizableThreeColumns
import XMonad.Util.EZConfig
import XMonad.Util.NamedScratchpad
import XMonad.Util.SpawnOnce (spawnOnce)
import XMonad.Util.Paste (sendKey)
import XMonad.Hooks.RefocusLast
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import qualified Data.Semigroup
import System.FilePath
import System.Environment
-- }}}

myTerminal :: [Char]
myTerminal = "alacritty"

myScriptDir :: FilePath
myScriptDir = "$XMONAD_HOME/scripts"

myCentreFloatRR :: W.RationalRect
myCentreFloatRR = W.RationalRect (1/6) (1/6) (2/3) (2/3)

myFullSizeRR :: W.RationalRect
myFullSizeRR = W.RationalRect 0 0 1 1

myMiniFloatRR :: W.RationalRect
myMiniFloatRR = W.RationalRect 0.64 0.63 0.35 0.35

-- { Workspaces } {{{
-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
-- A tagging example:
--
-- > workspaces = ["web", "irc", "code" ] ++ map show [4..9]
--

myWorkspaces :: [[Char]]
myWorkspaces = [ "\xf0ac"        -- 1: globe
               , "\xf121"        -- 2: code
               , "\xf04b"        -- 3: play
               , "\xf075"        -- 4: comment
               , "\xf02d"        -- 5: book
               , "\xf0e4"        -- 6: dashboard
               ]
-- }}}

-- { Keybindings } {{{
myKeys :: [([Char], X ())]
myKeys =
  -- { Basics } {{{
  -- [ ("M-<Return>" , spawn "alacritty --working-directory $(xcwd)")
  [ ("M-<Return>" , spawn "alacritty")
  , ("`"          , namedScratchpadAction myScratchPads "TermInScratchpad")
  , ("C-`"        , sendKey 0 xK_grave)
  , ("M-e"        , namedScratchpadAction myScratchPads "NeovideInScratchpad")
  , ("M-r"        , namedScratchpadAction myScratchPads "RangerInScratchpad")
  , ("M1-<Space>" , spawn $ myScriptDir </> "rofi/launchpad.sh")
  , ("<Print>"    , spawn $ myScriptDir </> "screenshot.sh")
  , ("M-q"        , kill)
  , ("M-<Space>"  , sendMessage NextLayout)
  -- , ("M-<Space>"  , sendMessage NextLayout >> (dynamicLogString def >>= \d->spawn $ "xmessage "++d))
  , ("M-S-b"      , spawn "polybar-msg cmd toggle")
  , ("M-b"        , sendMessage ToggleStruts)
  , ("M-S-q"      , io exitSuccess)
  , ("M-S-r"      , spawn $ myScriptDir </> "rebuild.sh")
  -- , ("<Pause>"    , spawn "systemctl suspend")
  ] ++
  -- }}}

  -- { Transparency } {{{
  [ ("M-S-=", spawn "picom-trans -c +10")
  , ("M-S--", spawn "picom-trans -c -10")
  ] ++
  -- }}}

  -- { Layout } {{{
  [ ("M1-<F1>"    , sendMessage $ JumpToLayout "tabbed")
  , ("M1-<F2>"    , sendMessage $ JumpToLayout "vtile")
  , ("M1-<F3>"    , sendMessage $ JumpToLayout "htile")
  , ("M1-<F4>"    , sendMessage $ JumpToLayout "full")
  , ("M1-<F5>"    , sendMessage $ JumpToLayout "grid")
  , ("M1-<F6>"    , sendMessage $ JumpToLayout "threeColumns")
  -- , ("M1-<F5>"    , sendMessage $ JumpToLayout "dualTab")
  ] ++
  -- }}}

  -- { Screen } {{{
  [ ("M-]"      , nextScreen)
  , ("M-["      , prevScreen)
  , ("M-S-]"    , swapNextScreen)
  , ("M-S-["    , swapPrevScreen)
  , ("M-`"    , swapPrevScreen)
  ] ++
  -- }}}

  -- { Window } {{{

  -- TODO: simplify the codes
  -- Focus
  [ ("M-j"           , windows W.focusDown)
  , ("M-s"           , windows W.focusDown)
  , ("M-<Page_Down>" , windows W.focusDown)
  , ("M1-<Tab>"      , windows W.focusDown)
  , ("M-k"           , windows W.focusUp)
  , ("M-w"           , windows W.focusUp)
  , ("M-<Page_Up>"   , windows W.focusUp)
  , ("M1-S-<Tab>"    , windows W.focusUp)
  -- , ("M-e"           , focusMaster)
  , ("M1-`"          , nextMatch History (return True))

  -- experiemental
  , ("M-S-e"           , selectWindow def >>= (`whenJust` windows . W.focusWindow))
  , ("M-S-o"           , windowMenu)
  -- , ("M1-<Tab>"      , toggleFocus)

  -- Swap
  , ("M-S-j"           , windows W.swapDown)
  , ("M-S-k"           , windows W.swapUp)
  , ("M-S-<Page_Down>" , windows W.swapDown)
  , ("M-S-<Page_Up>"   , windows W.swapUp)
  , ("M-m"             , dwmpromote)
  , ("M-S-m"           , windows W.swapMaster)

  -- -- Move
  -- , ("C-M1-h"         , sendMessage $ Move L)
  -- , ("C-M1-j"         , sendMessage $ Move D)
  -- , ("C-M1-k"         , sendMessage $ Move U)
  -- , ("C-M1-l"         , sendMessage $ Move R)
  , ("M-<L>", withFocused $ keysMoveWindow (-10,   0))
  , ("M-<R>", withFocused $ keysMoveWindow ( 10,   0))
  , ("M-<D>", withFocused $ keysMoveWindow (  0,  10))
  , ("M-<U>", withFocused $ keysMoveWindow (  0, -10))

  -- Resize
  , ("M-h"     , sendMessage Shrink)
  , ("M-l"     , sendMessage Expand)
  , ("M-S-h"   , sendMessage MirrorExpand)
  , ("M-S-l"   , sendMessage MirrorShrink)
  , ("M-S-<U>" , withFocused $ keysResizeWindow (20, 20) (1%2, 1%2))
  , ("M-S-<D>" , withFocused $ keysResizeWindow (-20, -20) (1%2, 1%2))
  -- , ("M-<L>"         , sendMessage Shrink)
  -- , ("M-<R>"         , sendMessage Expand)
  -- , ("M-<U>"         , sendMessage MirrorExpand)
  -- , ("M-<D>"         , sendMessage MirrorShrink)

  -- , ("M-S-f"         , sendMessage $ Toggle FULL)
  , ("M-t"           , withFocused $ windows . W.sink)
  , ("M-,"           , sendMessage (IncMasterN 1))
  , ("M-."           , sendMessage (IncMasterN (-1)))

  , ("C-M1-h"         , sendMessage $ pullGroup L)
  , ("C-M1-j"         , sendMessage $ pullGroup D)
  , ("C-M1-k"         , sendMessage $ pullGroup U)
  , ("C-M1-l"         , sendMessage $ pullGroup R)
  ] ++

  keysForFloating ++
  keysForControl ++
  -- }}}

  -- { Workspace } {{{

  -- navigation
  [ ("M-d"      , moveTo Next (hiddenWS :&: Not emptyWS :&: ignoringWSs [scratchpadWorkspaceTag]))
  , ("M-a"      , moveTo Prev (hiddenWS :&: Not emptyWS :&: ignoringWSs [scratchpadWorkspaceTag]))
  , ("M-g"      , toggleWindowSpacingEnabled)
  , ("M-<Tab>"  , toggleWS' ["NSP"])
  ] ++

  -- }}}

  -- { Clipboard } {{{
  [ ("M-c"  , clipboardCopy)
  , ("M-v"  , clipboardPaste)
  -- , ("M-x"  , clipboardCut)
  , ("M1-a" , ctrlA)
  , ("M1-c" , spawn "rofi -show clipboard -modi 'run,window,clipboard:greenclip print'")
  ]
  -- }}}

  where
        -- getSortByIndexNoSP = fmap (.namedScratchpadFilterOutWorkspace) getSortByIndex
        -- nonNSP             = WSIs (return (\ws -> W.tag ws /= "nsp"))
        -- nonEmptyNonNSP     = WSIs $ return $ \ws -> isJust (W.stack ws) && W.tag ws /= "nsp"
        -- hiddenNonEmptyNonNSP = WSIs $
        --     do hiddenWS <- gets (map W.tag . W.hidden . windowset)
        --        return $ \ws -> W.tag ws `elem` hiddenWS
        --                     && isJust (W.stack ws)
        --                     && W.tag ws /= "NSP"

        -- TODO simplify these codes
        clipboardCopy = withFocused $ \w -> do
            b <- isTerminal w
            if b
               then sendKey noModMask xF86XK_Copy
               else sendKey controlMask xK_c

        clipboardPaste = withFocused $ \w -> do
            b <- isTerminal w
            if b
                then sendKey noModMask xF86XK_Paste
                else sendKey controlMask xK_v

        -- clipboardCut = withFocused $ \w -> do
        --     b <- isTerminal w
        --     if b
        --         then sendKey noModMask xF86XK_Cut
        --         else sendKey controlMask xK_x

        ctrlA = withFocused $ \w -> do
            b <- isTerminal w
            if b
                then sendKey noModMask xK_Home
                else sendKey mod1Mask xK_a

        isTerminal = fmap ((== "Alacritty") <||> (isSuffixOf "Scratchpad")) . runQuery className

        keysForControl =
            -- volume control
            [ ("M-" ++ k, myVolumeCtrl a)
                | (k, a) <-
                [ ("="           , "up")
                , ("-"           , "down")
                , ("<Backspace>" , "mute")
                ]
            ] ++

            [ (k, myVolumeCtrl a)
                | (k, a) <-
                [ ("<XF86AudioRaiseVolume>", "up")
                , ("<XF86AudioLowerVolume>", "down")
                , ("<XF86AudioMute>", "mute")
                ]
            ] ++

            -- brightness
            [ ("M-" ++ k, myBrightnessCtrl a)
                | (k, a) <-
                [ ("0", "up")
                , ("9", "down")
                ]
            ] ++

            -- Ouput display
            [ ("M-o " ++ k, myMonitorCtrl a)
                | (k, a) <-
                [ ("m", "mirror")
                , ("n", "normal")
                , ("a", "arandr")
                , ("h", "hdmi")
                , ("d", "dual")
                ]
            ] ++

            -- exit
            [ ("M-x " ++ k, myExitCtrl a)
                | (k, a) <-
                [ ("s", "suspend")
                , ("z", "lock")
                , ("q", "logout")
                , ("r", "reboot")
                , ("d", "shutdown")
                ]
            ]
            where
                myVolumeCtrl arg = spawn $ myScriptDir </> "control-volume.sh " ++ arg
                myBrightnessCtrl arg = spawn $ myScriptDir </> "control-brightness.sh " ++ arg
                myMonitorCtrl arg = spawn $ myScriptDir </> "control-monitors.sh " ++ arg
                myExitCtrl arg = spawn $ myScriptDir </> "exit.sh " ++ arg

        keysForFloating =
            [ ("M-f"  , toggleCentreFloat)
            , ("M-M1-f"  , toggleMiniFloat)
            , ("M-S-f", withFocused $ windows . flip W.float myFullSizeRR)
            ] where
                floatOrNot f n = withFocused $ \windowId -> do
                    floats <- gets (W.floating . windowset)
                    if windowId `M.member` floats
                    then f
                    else n

                -- centreFloat w = windows $ W.float w (W.RationalRect 0.25 0.25 0.5 0.5)
                centreFloat w = windows $ W.float w myCentreFloatRR
                miniFloat w = windows $ W.float w myMiniFloatRR
                toggleCentreFloat = floatOrNot (withFocused $ windows . W.sink) (withFocused centreFloat)
                toggleMiniFloat = floatOrNot (withFocused $ windows . W.sink) (withFocused miniFloat)
-- }}}

-- { Border } {{{
myBorderWidth :: Dimension
myBorderWidth = 0

-- }}}

-- { Spacing } {{{
mySpacing :: l a -> ModifiedLayout Spacing l a
mySpacing = spacingRaw
  True                -- Only for >1 window
  (Border 1 1 1 1) -- Size of screen edge gaps (T B R L)
  True                -- Enable screen edge gaps
  (Border 3 3 3 3)    -- Size of window gaps
  True                -- Enable window gaps
-- }}}

-- { Hooks } {{{

-- { Layout } {{{
myTabConfig :: Theme
myTabConfig = def
  { fontName            = "xft:Monospace-10"
  , activeColor         = "#46d9ff"
  , activeBorderColor   = "#46d9ff"
  , activeTextColor     = "#282c34"
  , inactiveColor       = "#313846"
  , inactiveBorderColor = "#282c34"
  , inactiveTextColor   = "#d0d0d0"
  }

commonLayoutSetting = windowNavigation
  . windowArrange
  . mouseResize
  . trackFloating
  . mySpacing
  . avoidStruts

-- apply commonLayoutSetting beforehand to resolve the conflict between tall & tabbed layouts
myLayoutHook = commonLayoutSetting $ myTabbedLayout
                                 ||| myVTiledLayout
                                 ||| myHTiledLayout
                                 ||| myFullLayout
                                 ||| myGridLayout
                                 ||| myThreeColumnLayout

    -- We need to place spacing after renamed switch the layouts normally
    where
        myVTiledLayout =
          renamed [Replace "vtile"]
          $ mkToggle (NOBORDERS ?? FULL ?? EOT)
          $ addTabs shrinkText myTabConfig
          $ subLayout [] (smartBorders Simplest)
          $ ResizableTall
            1        -- number of master panes
            (3/100)  -- % of screen to increment by when resizing
            (1/2)    -- ratio of the master pane
            []

        myHTiledLayout =
          renamed [Replace "htile"]
          $ Mirror myVTiledLayout

        -- Don't add spacing, otherwise the movement of focus would fail
        myTabbedLayout =
          renamed [Replace "tabbed"]
          $ addTabs shrinkText myTabConfig
          $ tabbedBottom shrinkText myTabConfig

        myFullLayout = renamed [Replace "full"] Full

        myThreeColumnLayout =
          renamed [Replace "threeColumns"]
          $ ResizableThreeColMid 1 (3/100) (1/2) []

        myGridLayout = renamed [Replace "grid"] $ GridRatio (3/2)

        -- myDualTabLayout =
        --   renamed [Replace "dualTab"]
        --   $ mySpacing
        --   $ combineTwo (TwoPane 0.03 0.5) myTabbedLayout myTabbedLayout
-- }}}

-- { Manage Hook } {{{
ruleManageHook :: Query (Data.Semigroup.Endo WindowSet)
ruleManageHook = composeAll
  [ resource  =? "desktop_window" --> doIgnore
  , resource  =? "kdesktop"       --> doIgnore
  , isFullscreen                  --> doFullFloat
  , isDialog                      --> doCenterFloat
  ]

workspaceManageHook :: Query (Data.Semigroup.Endo WindowSet)
workspaceManageHook = composeAll
  [ rule --> viewShift ( myWorkspaces !! idx ) | (idx, rule) <-
    [ (0, className =? "firefox")
    , (2, className =? "mpv")
    , (2, className =? "Sxiv")
    , (3, title =? "Messenger")
    , (3, title =? "LINE")
    , (3, className =? "discord")
    , (3, className =? "Slack")
    , (4, className =? "Inkscape")
  ]] where
    viewShift = doF . liftM2 (.) W.greedyView W.shift

floatingManageHook :: Query (Data.Semigroup.Endo WindowSet)
floatingManageHook = composeAll
  [ className =? n --> doCenterFloat | n <- classNames ]
  where classNames = [ "MPlayer"
                     , "Gimp"
                     , "Thunar"
                     ]

myManageHook :: Query (Data.Semigroup.Endo WindowSet)
-- myManageHook = insertPosition Above Newer
myManageHook = ruleManageHook
  <+> ruleManageHook
  <+> namedScratchpadManageHook myScratchPads
  <+> floatingManageHook
  <+> workspaceManageHook
-- }}}

-- { Event Hook } {{{
myEventHook :: Event -> X Data.Semigroup.All
myEventHook = refocusLastWhen myPred
    <+> fadeWindowsEventHook
    -- <+> fullscreenEventHook
    -- <+> swallowEventHook (className =? "Alacritty") (return True)
    where
        myPred = refocusingIsActive <||> isFloat
-- }}}

-- { Log Hook } {{{
myFadeHook :: FadeHook
myFadeHook = composeAll
    [ opaque -- default to opaque
    -- , isUnfocused --> opacity 0.95
    -- , (className =? "Alacritty") <&&> isUnfocused --> opacity 0.87
    -- , (className =? "NeovideInScratchpad") --> opacity 0.85
    -- , fmap ("Google" `isPrefixOf`) className --> opaque
    , isDialog --> opaque
    --, isUnfocused --> opacity 0.55
    --, isFloating  --> opacity 0.75
    ]
myLogHook :: X ()
myLogHook = refocusLastLogHook
  <+> historyHook
  <+> nsHideOnFocusLoss myScratchPads
  -- <+> hideOnFocusChange myScratchPads
  <+> fadeWindowsLogHook myFadeHook
  <+> updatePointer (0.5, 0.5) (0, 0)
-- }}}

-- { Startup Hook } {{{
mySetEnv :: IO ()
mySetEnv = do
  userHome <- getEnv "HOME"
  setEnv "XMONAD_HOME" $ userHome </> ".xmonad"

myStartupHook :: X ()
myStartupHook = do
  liftIO mySetEnv
  spawnOnce "$HOME/.config/polybar/spwan.sh"
  setWMName "LG3D"
-- }}}

-- }}}

-- { Scratch Pad } {{{
myScratchPads :: [NamedScratchpad]
myScratchPads =
  [ NS "TermInScratchpad" spawnTerm (resource =? "TermInScratchpad") fullSize
  , NS "RangerInScratchpad" spawnRanger (resource =? "RangerInScratchpad") halfSize
  , NS "NeovideInScratchpad" spawnNeovide (className =? "NeovideInScratchpad") halfSize
  ] where
    spawnTerm = "alacritty --class=TermInScratchpad"
    spawnRanger = "alacritty --class=RangerInScratchpad -e ranger"
    spawnNeovide = "neovide --x11-wm-class=NeovideInScratchpad"
    halfSize = customFloating myCentreFloatRR
    fullSize = customFloating myFullSizeRR

-- myScratchPads =
--   [ NS keyword command condition  size
--     | (command, keyword, size) <-
--     [ ("alacritty --class=TermInScratchpad", "TermInScratchpad", (resource =? keyword), fullSize)
--     , ("alacritty --class=RangerInScratchpad -e ranger", "RangerInScratchpad", halfSize)
--     , ("neovide --x11-wm-class=NeovideInScratchpad", "NeovideInScratchpad", halfSize)
--     ]
--   ]
--   where
--     halfSize = customFloating myCentreFloatRR
--     fullSize = customFloating myFullSizeRR


-- -- Ref: https://www.reddit.com/r/xmonad/comments/o3i7st/hiding_scratchpads_when_loosing_focus/
-- hideOnFocusChange :: NamedScratchpads -> X ()
-- scratchpadWorkspaceTag = "NSP"
-- hideOnFocusChange scratches = withWindowSet $ \winSet -> do
--     let cur = W.currentTag winSet
--     withRecentsIn cur () $ \lastFocus _ ->
--         when (lastFocus `elem` W.index winSet && cur /= scratchpadWorkspaceTag) $
--             whenX (isNS lastFocus) $
--                 shiftToNSP (W.workspaces winSet) ($ lastFocus)
--   where
--     isNS :: Window -> X Bool
--     isNS w = or <$> traverse ((`runQuery` w) . query) scratches

--     withRecentsIn :: WorkspaceId -> a -> (Window -> Window -> X a) -> X a
--     withRecentsIn tag dflt f = maybe (return dflt) (\(Recent lw cw) -> f lw cw)
--                              . Map.lookup tag
--                              . (\(RecentsMap m) -> m)
--                            =<< XS.get

--     shiftToNSP :: [WindowSpace] -> ((Window -> X ()) -> X ()) -> X ()
--     shiftToNSP ws f = do
--         unless (any ((scratchpadWorkspaceTag ==) . W.tag) ws) $
--             addHiddenWorkspace scratchpadWorkspaceTag
--         f (windows . W.shiftWin scratchpadWorkspaceTag)
-- }}}

-- { Main } {{{
main :: IO ()
main = xmonad
    $ withUrgencyHook NoUrgencyHook
    $ ewmhFullscreen . ewmh
    $ docks (additionalKeysP defaults myKeys)
      where
        defaults = def
          { terminal             = myTerminal
          ,   borderWidth        = myBorderWidth
          ,   modMask            = mod4Mask
          ,   workspaces         = myWorkspaces
          ,   layoutHook         = myLayoutHook
          ,   manageHook         = myManageHook
          ,   handleEventHook    = myEventHook
          ,   logHook            = myLogHook
          ,   startupHook        = myStartupHook
          ,   focusFollowsMouse  = True
          -- ,   keys               = myKeys
          -- ,   mouseBindings      = myMouseBindings
          }
-- }}}

-- vim:foldmethod=marker
