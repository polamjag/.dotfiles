function ghq-look() {
  cd "$(ghq list --exact --full-path $1)"
}

function _ghq-look() {
  compadd $(ghq list --unique)
}

compdef _ghq-look ghq-look
