# coloring for ls
if [ -e ~/.dir_colors ] ; then
  eval $(dircolors -b ~/.dir_colors)
fi

alias ls='ls -F --color'
alias l='ls -Ca'
alias ll='ls -lhF'
alias lla='ls -lhaF'
alias lar='l -R'
alias lr='ls -R'
alias llr='ll -R'
alias llar='lla -R'
