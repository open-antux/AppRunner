# AppRunner
A simple launcher written in haskell for xmonad.

## Why?
I was simply tired of [rofi](https://github.com/davatorium/rofi), I wanted 
something that worked like rofi but more minimalist written in Haskell.

### Goals
- [x] Include search predicate.
- [x] Add support for arguments.
  - [ ] Add support for `ftp` and `sftp` protocol
- [ ] Use `Text` instead of `String`.
- [x] Include the paths described in `$XDG_DATA_DIRS`.
- [ ] Take into consideration all of the desktop entry options (actually it consider 
`Name`, `NoDisplay`, `Hidden`, `Terminal` and `Exec`).

## How to configure

### Import AppRunner.hs
First of all, it's necessary to clone and import AppRunner as external library 
into the `.xmonad` config directory:
```bash
$ mkdir -p .xmonad/lib
$ cd .xmonad/lib
$ git clone https://github.com/open-antux/AppRunner.git 
$ ln -s AppRunner/AppRunner.hs . # Link is necessary otherwise xmonad can't see the library
```

Then you can import the [`XPrompt`](https://hackage.haskell.org/package/xmonad-contrib-0.13/docs/XMonad-Prompt.html)
library:
```haskell
import XMonad.Prompt                                                            
    ( XPPosition(Top),                                                          
      XPConfig(font, bgColor, fgColor, fgHLight, bgHLight, borderColor,         
               promptBorderWidth, position, alwaysHighlight, height, maxComplRows,
               historySize, historyFilter, promptKeymap, completionKey,         
               changeModeKey, defaultText, autoComplete, showCompletionOnTab,   
               searchPredicate, sorter ),                                       
      defaultXPKeymap )                                                         

-- {{ Custom import }}                                                          
import AppRunner
```

### launchAppPrompt
In order to launch the AppRunner launcher, you have to set the keybinding:
```haskell
myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig {XMonad.modMask = modm} = M.fromList $
    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch the AppRunner prompt
    , ((modm,               xK_p     ), launchAppPrompt promptConfig)

    -- other keybindings
    , ...
```

### Configure XPrompt
```haskell
promptConfig :: XPConfig
promptConfig = def {
         font                = "xft:Misc Fixed:style=Regular:pixelsize=12"
       , bgColor             = "#000000"
       , fgColor             = "#ffffff"
       , fgHLight            = "#00ff00"
       , bgHLight            = "#000000"
       , borderColor         = "#000000"
       , promptBorderWidth   = 1
       , position            = Top
       , alwaysHighlight     = True
       , height              = 18
       , maxComplRows        = Nothing
       , historySize         = 0
       , historyFilter       = id
       , promptKeymap        = defaultXPKeymap
       , completionKey       = (0, xK_Tab)
       , changeModeKey       = xK_grave
       , defaultText         = []
       , autoComplete        = Nothing
       , showCompletionOnTab = False
       , searchPredicate     = fuzzyMatch
       , sorter              = fuzzySort
}
```
That's the configuration that you can see in the example, I highly reccomend to
import the [FuzzyMatch](https://hackage.haskell.org/package/xmonad-contrib-0.16/docs/XMonad-Prompt-FuzzyMatch.html)
library in order to have a better research and sort.

#### Example work
<img alt="Example usage" src="https://github.com/open-antux/AppRunner/blob/332ff98a142bdee66f482b7dddcbdf180e9ddab4/examples/example_use.gif?raw=true" width="100%"/>
