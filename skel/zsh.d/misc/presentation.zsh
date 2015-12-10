presentation-mode() {
  echo
  echo "Entering presentation mode on $(date)"
  echo

  unset RPROMPT
  PROMPT='%B%~%b%# '
}
