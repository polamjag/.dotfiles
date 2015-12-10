if $(! $(hash open &>/dev/null)) && $(hash oopen &>/dev/null) ; then
  alias open=oopen
fi

alias cal='cal -s'

ulimit -s unlimited
