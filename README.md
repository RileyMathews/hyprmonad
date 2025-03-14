# hyprmonad
A CLI for hyprland to save and restore monitor profiles inspired by autorandr.

## Installation
Currently the only way to install is compiling from source. Stack is recommended
but if you know cabal you can likely get that working as well.

Make sure you have stack installed and run `stack install`

## Usage
```bash
hyprmonad save profileName # saves the current monitor layout as profileName
hyprmonad load profileName # loads the profile
hyprmonad list # list profiles
```
