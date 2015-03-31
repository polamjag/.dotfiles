#!/bin/sh

# c.f.
# http://rcmdnk.github.io/blog/2015/03/22/computer-mac/
# http://tukaikta.blog135.fc2.com/blog-entry-251.html

# suppress .DS_Store
defaults write com.apple.desktopservices DSDontWriteNetworkStores true
# disable shadow on screen capture
defaults write com.apple.screencapture "disable-shadow" -bool yes
# text becomes selectable in quicklook
defaults write com.apple.finder QLEnableTextSelection -bool true
# more items on "recent folders"
defaults write -g NSNavRecentPlacesLimit -int 10
# remove delay of Dock
defaults write com.apple.Dock autohide-delay -float 0
# modify filename of screen shot file
defaults write com.apple.screencapture name scr


# make mac quiet on boot
sudo nvram SystemAudioVolume=%80

# git
git config --global core.precomposeunicode true
