#!/bin/bash
# unit tests for setup.sh
# usage: bash test_setup.sh
#
# Each test runs setup.sh as a subprocess with HOME pointed at a temporary
# directory, so the real home directory is never touched.
# Commands requiring network/external tools (lib, vim, emacs, update) are
# out of scope.

cd "$(dirname "$0")"
REPO_DIR="$PWD"
SETUP_SH="${REPO_DIR}/setup.sh"

pass_count=0
fail_count=0

# run setup.sh with the temporary HOME; stdin is inherited from the caller
run_setup() {
  HOME="$TEST_HOME" bash "$SETUP_SH" "$@"
}

# assertions: print a message to stderr and return 1 on failure
assert_eq() {
  if [ "$1" != "$2" ]; then
    echo "  assertion failed: $3" >&2
    echo "    expected: $1" >&2
    echo "    actual:   $2" >&2
    return 1
  fi
}

assert_exists() {
  if [ ! -e "$1" ]; then
    echo "  assertion failed: $1 does not exist" >&2
    return 1
  fi
}

assert_not_exists() {
  if [ -e "$1" ] || [ -L "$1" ]; then
    echo "  assertion failed: $1 should not exist" >&2
    return 1
  fi
}

assert_symlink() {
  if [ ! -L "$1" ]; then
    echo "  assertion failed: $1 is not a symlink" >&2
    return 1
  fi
  local actual
  actual="$(readlink "$1")"
  if [ "$actual" != "$2" ]; then
    echo "  assertion failed: $1 -> $actual (expected -> $2)" >&2
    return 1
  fi
}

assert_not_symlink() {
  if [ -L "$1" ]; then
    echo "  assertion failed: $1 should not be a symlink" >&2
    return 1
  fi
}

assert_file_eq() {
  if ! cmp -s "$1" "$2"; then
    echo "  assertion failed: $1 and $2 differ" >&2
    return 1
  fi
}

assert_contains() {
  case "$1" in
    *"$2"*) return 0 ;;
  esac
  echo "  assertion failed: expected to contain: $2" >&2
  echo "    actual: $1" >&2
  return 1
}

# tests
test_usage_shows_help() {
  local output
  output="$(run_setup --usage < /dev/null 2>&1)"
  assert_contains "$output" "Usage:"
}

test_unknown_command_fails() {
  local output status=0
  output="$(run_setup bogus < /dev/null 2>&1)" || status=$?
  assert_eq "1" "$status" "exit code of unknown command"
  assert_contains "$output" "Unknown command: bogus"
}

test_dot_links_skel_files() {
  run_setup dot < /dev/null > /dev/null
  assert_symlink "$TEST_HOME/.zshrc" "$REPO_DIR/skel/zshrc"
  assert_symlink "$TEST_HOME/.vimrc" "$REPO_DIR/skel/vimrc"
  assert_symlink "$TEST_HOME/.tmux.conf" "$REPO_DIR/skel/tmux.conf"
  assert_symlink "$TEST_HOME/.emacs.d" "$REPO_DIR/skel/emacs.d"
}

test_dot_excludes_special_files() {
  run_setup dot < /dev/null > /dev/null
  assert_not_exists "$TEST_HOME/.gitconfig.local"
  assert_not_exists "$TEST_HOME/.zshenv.exam"
  assert_not_exists "$TEST_HOME/..vimrc.ext.un~"
}

test_dot_copies_zshenv_and_writes_npmrc() {
  run_setup dot < /dev/null > /dev/null
  assert_not_symlink "$TEST_HOME/.zshenv"
  assert_file_eq "$TEST_HOME/.zshenv" "$REPO_DIR/skel/zshenv.exam"
  assert_exists "$TEST_HOME/.npmrc"
  assert_contains "$(cat "$TEST_HOME/.npmrc")" "prefix="
}

test_dot_prompt_no_keeps_existing_file() {
  echo "my zshrc" > "$TEST_HOME/.zshrc"
  printf 'n\n' | run_setup dot > /dev/null
  assert_not_symlink "$TEST_HOME/.zshrc"
  assert_eq "my zshrc" "$(cat "$TEST_HOME/.zshrc")" "content of .zshrc"
  assert_not_exists "$TEST_HOME/.zshrc.old"
}

test_dot_prompt_yes_renames_and_links() {
  echo "my zshrc" > "$TEST_HOME/.zshrc"
  printf 'y\n' | run_setup dot > /dev/null
  assert_symlink "$TEST_HOME/.zshrc" "$REPO_DIR/skel/zshrc"
  assert_eq "my zshrc" "$(cat "$TEST_HOME/.zshrc.old")" "content of .zshrc.old"
}

test_dot_force_overwrites_without_asking() {
  echo "my zshrc" > "$TEST_HOME/.zshrc"
  run_setup --force dot < /dev/null > /dev/null
  assert_symlink "$TEST_HOME/.zshrc" "$REPO_DIR/skel/zshrc"
  assert_not_exists "$TEST_HOME/.zshrc.old"
}

test_dot_rerun_is_idempotent() {
  run_setup dot < /dev/null > /dev/null
  local output
  output="$(run_setup dot < /dev/null 2>&1)"
  assert_contains "$output" "Already linked: $TEST_HOME/.zshrc"
  assert_symlink "$TEST_HOME/.zshrc" "$REPO_DIR/skel/zshrc"
}

test_dot_renames_existing_real_directory() {
  mkdir -p "$TEST_HOME/.emacs.d"
  echo "(init)" > "$TEST_HOME/.emacs.d/init.el"
  printf 'y\n' | run_setup dot > /dev/null
  assert_symlink "$TEST_HOME/.emacs.d" "$REPO_DIR/skel/emacs.d"
  assert_eq "(init)" "$(cat "$TEST_HOME/.emacs.d.old/init.el")" "content of .emacs.d.old/init.el"
}

test_git_creates_config_from_input() {
  printf 'alice\nalice@example.com\n' | run_setup git > /dev/null
  assert_exists "$TEST_HOME/.gitconfig.local"
  assert_contains "$(cat "$TEST_HOME/.gitconfig.local")" "name = alice"
  assert_contains "$(cat "$TEST_HOME/.gitconfig.local")" "email = alice@example.com"
}

test_git_copies_skeleton_on_empty_input() {
  run_setup git < /dev/null > /dev/null
  assert_file_eq "$TEST_HOME/.gitconfig.local" "$REPO_DIR/skel/gitconfig.local"
}

test_git_skips_existing_config() {
  echo "keep me" > "$TEST_HOME/.gitconfig.local"
  run_setup git < /dev/null > /dev/null
  assert_eq "keep me" "$(cat "$TEST_HOME/.gitconfig.local")" "content of .gitconfig.local"
}

test_bin_links_scripts() {
  run_setup bin < /dev/null > /dev/null
  assert_symlink "$TEST_HOME/bin/git-ver" "$REPO_DIR/bin/git-ver"
  assert_symlink "$TEST_HOME/bin/peco-ghq" "$REPO_DIR/bin/peco-ghq"
}

test_binx_links_scripts() {
  run_setup binx < /dev/null > /dev/null
  assert_symlink "$TEST_HOME/bin/kmap" "$REPO_DIR/bin_x/kmap"
}

test_zed_links_config_dir() {
  run_setup zed < /dev/null > /dev/null
  assert_symlink "$TEST_HOME/.config/zed" "$REPO_DIR/config/zed"
}

test_ghostty_links_config_file() {
  run_setup ghostty < /dev/null > /dev/null
  assert_symlink "$TEST_HOME/.config/ghostty/config" "$REPO_DIR/config/ghostty"
}

test_ghostty_prompt_no_keeps_existing_file() {
  mkdir -p "$TEST_HOME/.config/ghostty"
  echo "my config" > "$TEST_HOME/.config/ghostty/config"
  printf 'n\n' | run_setup ghostty > /dev/null
  assert_not_symlink "$TEST_HOME/.config/ghostty/config"
  assert_eq "my config" "$(cat "$TEST_HOME/.config/ghostty/config")" "content of ghostty config"
}

test_interactive_mode_all_declined() {
  run_setup < /dev/null > /dev/null
  assert_not_exists "$TEST_HOME/.zshrc"
  assert_not_exists "$TEST_HOME/bin"
}

# runner
run_test() {
  local name="$1" output
  TEST_HOME="$(mktemp -d)"
  if output="$(set -e; "$name" 2>&1)"; then
    printf 'ok      %s\n' "$name"
    pass_count=$((pass_count + 1))
  else
    printf 'not ok  %s\n' "$name"
    echo "$output" | sed 's/^/    /'
    fail_count=$((fail_count + 1))
  fi
  rm -rf "$TEST_HOME"
}

for test_fn in $(compgen -A function test_); do
  run_test "$test_fn"
done

echo
echo "passed: ${pass_count}, failed: ${fail_count}"
[ "$fail_count" -eq 0 ]
