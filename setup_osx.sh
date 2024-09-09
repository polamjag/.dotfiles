#!/bin/sh

set -ex

# c.f.
# http://rcmdnk.github.io/blog/2015/03/22/computer-mac/
# http://tukaikta.blog135.fc2.com/blog-entry-251.html

# suppress .DS_Store
defaults write com.apple.desktopservices DSDontWriteNetworkStores -bool true
defaults write com.apple.desktopservices DSDontWriteUSBStores -bool true
# disable shadow on screen capture
defaults write com.apple.screencapture "disable-shadow" -bool yes
# save screenshot to ~/Downloads
defaults write com.apple.screencapture location ~/Downloads
#
defaults write com.apple.screencapture "show-thumbnail" -bool no
# text becomes selectable in quicklook
defaults write com.apple.finder QLEnableTextSelection -bool true
# more items on "recent folders"
defaults write -g NSNavRecentPlacesLimit -int 10
# remove delay of Dock
defaults write com.apple.Dock autohide-delay -float 0
# modify filename of screen shot file
defaults write com.apple.screencapture name scr
# change speed of mission control's animation speed
#defaults write com.apple.dock expose-animation-duration -float 0.25

# https://stackoverflow.com/questions/39972335/how-do-i-press-and-hold-a-key-and-have-it-repeat-in-vscode
defaults write com.microsoft.VSCode ApplePressAndHoldEnabled -bool false
defaults write com.microsoft.VSCodeInsiders ApplePressAndHoldEnabled -bool false

killall Dock
killall SystemUIServer

# git
git config --global core.precomposeunicode true

defaults write NSGlobalDomain NSAutomaticQuoteSubstitutionEnabled -bool false
defaults write NSGlobalDomain NSAutomaticDashSubstitutionEnabled -bool false

