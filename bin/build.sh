#!/bin/sh
#
# This script may look ugly, but that's because it is POSIX Shell
# Command Language compliant:
#
#   http://pubs.opengroup.org/onlinepubs/9699919799/
#
# The main issue is the lack of arrays, so the only way to build a
# command-line without issues with whitespace is to use the `set'
# built-in.

set -e

SCRIPT="$0"
SCRIPT_NAME=$(basename "$0")
BINDIR=$(dirname "$0")
ROOT=$(readlink -en "${BINDIR}/../")
BUILD_ID="nil"

usage() {
  echo "Usage: $SCRIPT_NAME {operation|implementation}*"
  echo
  echo "  operation:      build|test (default: build)"
  echo "  implementation: sbcl|ccl (default: sbcl)"
}

calculate_build_id() {
  local rel_tag
  local n_master
  local n_branch
  local hash
  local release
  local build_id
  local git="git -C ${ROOT}"

  rel_tag=$($git describe --tags --match="rel-*" --abbrev=0 master 2> /dev/null)\
    || {
    echo "No release tag found, cannot calculate build id." >&2
    return
  }

  n_master=$($git rev-list master --not "${rel_tag}" --count)
  n_branch=$($git rev-list HEAD --not master --count)
  release="${rel_tag#rel-}"
  build_id="${release}"

  if [ ! "$n_master" -eq 0 ]; then
    build_id="${release}.${n_master}"
  fi

  hash=$($git rev-parse --short HEAD)

  if [ $n_branch -eq 0 ]; then
    build_id="${build_id}-${hash}"
  else
    branch=$($git rev-parse --abbrev-ref HEAD)
    build_id="${build_id}.${branch}.${n_branch}-${hash}"
  fi

  BUILD_ID="\"${build_id}\""
}

OPERATION="build"
LISP="sbcl"

while [ 0 -lt $# ] ; do
  case "$1" in
    sbcl)
      LISP=sbcl
      ;;
    ccl)
      LISP=ccl
      ;;
    test)
      OPERATION=test
      ;;
    load-deps)
      OPERATION=load-deps
      ;;
    --help|-h)
      usage
      exit 0
      ;;
    *)
      echo "$SCRIPT: Unrecognized option '$1'."
      echo "Try '$SCRIPT --help' for more information."
      exit 1
      ;;
  esac
  shift
done

# Implementation-specific options.
case $LISP in
  sbcl)
    set -- \
        --dynamic-space-size 2048 \
        --no-sysinit \
        --no-userinit \
        --eval "(sb-ext:disable-debugger)" \
        --eval "(sb-ext:restrict-compiler-policy 'safety 1)" \
        --eval "(sb-ext:restrict-compiler-policy 'debug 1)"
    ;;
  ccl)
    set -- --batch --no-init
    ;;
esac

# Common options.
set -- "$@" \
    --eval "(push :hunchentoot-no-ssl *features*)" \
    --eval "(require :asdf)"

# Build options.
case "$OPERATION" in
  test)
    case $LISP in
      sbcl)
        set -- "$@" \
            --eval "(proclaim '(optimize (debug 3) (safety 3)))" \
            --eval "(asdf:test-system \"pastelyzer\")" \
            --eval "(sb-ext:exit :code (if 2am::*fail-count* 1 0))"
        ;;
      ccl)
        set -- "$@" \
            --eval "(proclaim '(optimize (debug 3) (safety 3)))" \
            --eval "(asdf:test-system \"pastelyzer\")" \
            --eval "(ccl:quit (if 2am::*fail-count* 1 0))"
        ;;
    esac
    ;;
  load-deps)
    set -- "$@" \
        --load ~/quicklisp/setup.lisp \
        --eval "(ql:quickload :swank)" \
        --eval "(push '#:swank-indentation swank-loader::*contribs*)" \
        --eval "(swank-loader:init :load-contribs t :setup nil :reload t)" \
        --eval "(ql:quickload \"pastelyzer\")" \
        --quit
    ;;
  *)
    calculate_build_id
    set -- "$@" \
        --eval "(asdf:load-system :swank)" \
        --eval "(push '#:swank-indentation swank-loader::*contribs*)" \
        --eval "(swank-loader:init :load-contribs t :setup nil :reload t)" \
        --eval "(asdf:load-system \"pastelyzer\")" \
        --eval "(setq pastelyzer::*build-id* ${BUILD_ID})" \
        --eval "(proclaim '(optimize (speed 2) (safety 1)))" \
        --eval "(asdf:make \"pastelyzer\")"
    if [ -f "${BINDIR}/pastelyzer" ]; then
      mv -f "${BINDIR}/pastelyzer" "${BINDIR}/pastelyzer.old"
    fi
    ;;
esac

export CL_SOURCE_REGISTRY="${ROOT}/:${ROOT}/deps//:~/quicklisp/dists/quicklisp/software//"

"$LISP" "$@"
