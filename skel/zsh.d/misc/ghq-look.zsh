function ghq-look() {
  cd $(ghq list --full-path --exact $0)
}

function _ghq-look() {
  compadd $(ghq list --unique)
}

compdef _ghq-look ghq-look
