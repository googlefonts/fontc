set -o nounset
set -o errexit
set -o pipefail

# Colors courtesy of https://stackoverflow.com/a/20983251
TEXT_RESET=$(tput sgr0)
COLOR_RED=$(tput setaf 1)

function die_with_usage() {
    >&2 echo "${COLOR_RED}${1}${TEXT_RESET}"
    usage
    exit 1
}

# Crates are named as cargo knows them, not by directory: some are nested
function validate_crates() {
    local names
    names=$(cargo metadata --format-version 1 --no-deps | jq -r '.packages[].name')
    for crate in "$@"
    do
      grep -qxF -- "$crate" <<< "$names" || die_with_usage "Invalid crate: ${crate}"
    done
}
