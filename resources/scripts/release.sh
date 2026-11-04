#!/bin/bash
# See usage() for usage. Needs jq, curl and cargo >= 1.90.

source "$(dirname -- "$0";)/rel-common.sh"

# Helpers
function usage() {
  echo "Usage:"
  echo "       # release every crate whose current version isn't published yet"
  echo "       ./release.sh"
  echo
  echo "       # release one crate"
  echo "       ./release.sh fontc"
  echo
  echo "Run this after bump-version.sh, from a fresh worktree of the up-to-date"
  echo "main: cargo packages whatever is on disk."
}

# "name version" for each workspace crate not marked `publish = false`
function workspace_crates() {
  set -o pipefail
  cargo metadata --format-version 1 --no-deps \
    | jq -r '.packages[] | select(.publish != []) | "\(.name) \(.version)"'
}

function is_published() {
  local status
  status=$(curl -s -o /dev/null -w '%{http_code}' \
    -A "fontc release.sh (https://github.com/googlefonts/fontc)" \
    "https://crates.io/api/v1/crates/$1/$2")
  case "${status}" in
    200) return 0 ;;
    404) return 1 ;;
    *) echo "${COLOR_RED}crates.io returned ${status} for $1 $2${TEXT_RESET}" >&2; exit 1 ;;
  esac
}

# What is it you want us to do?
if [ $# -gt 1 ]; then
  die_with_usage "Specify 0 - meaning all - or 1 packages"
fi
if [ $# -eq 1 ]; then
  validate_crates "$1"
fi

git fetch -q origin main
if [ "$(git rev-parse HEAD)" != "$(git rev-parse origin/main)" ]; then
  die_with_usage "HEAD is not origin/main; release from a fresh worktree of the up-to-date main"
fi

# errexit doesn't see a failure inside a process substitution, so read the
# crates first: if this fails, we stop here.
crates=$(workspace_crates)

to_publish=()
to_tag=()
while read -r name version; do
  if [ -z "${name}" ] || { [ $# -eq 1 ] && [ "${name}" != "$1" ]; }; then
    continue
  fi
  if ! is_published "${name}" "${version}" < /dev/null; then
    to_publish+=("${name}")
  fi
  # A rerun after a failed publish still tags the crates published before it failed
  tag="${name}-v${version}"
  if [ -z "$(git ls-remote --tags origin "refs/tags/${tag}" < /dev/null)" ]; then
    to_tag+=("${name} ${version}")
  fi
done <<< "${crates}"

if [ ${#to_publish[@]} -eq 0 ] && [ ${#to_tag[@]} -eq 0 ]; then
  echo "Nothing to release: every version is published and tagged."
  exit 0
fi

# Do the thing. We set errexit (in rel-common.sh) so step failure should break us out.

if [ ${#to_publish[@]} -gt 0 ]; then
  package_args=()
  for name in "${to_publish[@]}"; do
    package_args+=(-p "${name}")
  done

  # cargo publish (unlike cargo release) verifies each crate against the ones
  # packaged with it, so crates depending on newly bumped siblings verify too,
  # and uploads them in dependency order.
  echo "Dry run: ${to_publish[*]}"
  cargo publish --dry-run "${package_args[@]}"

  echo -n "Publish ${to_publish[*]} to crates.io? (y/N): "
  read -r response
  [[ "$response" =~ ^[Yy]$ ]] || exit 1
  cargo publish "${package_args[@]}"
fi

if [ ${#to_tag[@]} -gt 0 ]; then
  head=$(git rev-parse HEAD)
  echo "Tagging ${head}"
  tags=()
  for entry in "${to_tag[@]}"; do
    read -r name version <<< "${entry}"
    tag="${name}-v${version}"
    if target=$(git rev-parse -q --verify "refs/tags/${tag}^{commit}"); then
      # left over from an earlier attempt: fine only if it's on this commit
      if [ "${target}" != "${head}" ]; then
        echo "${COLOR_RED}Local tag ${tag} points to ${target}, not HEAD; delete it or check out ${target}${TEXT_RESET}" >&2
        exit 1
      fi
    else
      git tag -a "${tag}" -m "chore: Release ${name} version ${version}"
    fi
    tags+=("${tag}")
  done

  # One push per tag: GitHub sends no push events when more than three tags
  # are pushed at once, and release.yml runs on tag pushes. Pushing explicit
  # refs also leaves out any tags fetched from other remotes.
  echo -n "Push ${tags[*]} to origin? (y/N): "
  read -r response
  [[ "$response" =~ ^[Yy]$ ]] || exit 1
  for tag in "${tags[@]}"; do
    git push origin "refs/tags/${tag}"
  done
fi

echo "NEXT STEPS"
echo "Check that release.yml ran for the fontc and otl-normalizer tags; if one"
echo "didn't, run it on the tag, e.g."
echo "    gh workflow run release.yml --ref fontc-vX.Y.Z -f tool=fontc"
echo "Then edit the GitHub release notes."
