#!/bin/bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"
set -e

if [ -z "$1" ] || [ -z "$2" ]; then
    echo "Usage: $0 <version-commit> <version-number>"
    exit 1
fi
cd "$(git rev-parse --show-toplevel)"

if [ -z "$(git status --porcelain)" ]; then 
  echo "No uncommitted changes, proceeding"
else 
  echo "Uncommitted changes, please commit or stash before proceeding"
  exit 1
fi

# ensure tags don't exist yet
if git rev-parse -q --verify "refs/tags/$2" >/dev/null; then
  echo "Tag $2 already exists"
  exit 1
fi
if git rev-parse -q --verify "refs/tags/v$2-release" >/dev/null; then
  echo "Tag $2 already exists"
  exit 1
fi
# ensure release branch doesn't exist yet
if git rev-parse -q --verify "refs/heads/release-$2" >/dev/null; then
  echo "Branch release-$2 already exists"
  exit 1
fi

# tag the release on main
git tag -a "$2" $(git show-ref -s $1 | head -1) -m "Main commit of released version $2"

# Create release
git checkout --orphan "release-$2" "$1"

# pre-release cleanup
cp "${DIR}/.Rprofile.release" .Rprofile

sed -i .bak 's#"https://rspm.apps.dit-prdocp.novartis.net[^"]+"#"https://cloud.r-project.org"#' renv.lock
sed -i .bak "s/Version: .*/Version: $2/" DESCRIPTION

rm -rf .gitlab-ci.yml
rm -f *.bak
rm -rf inst/release
rm -f cran-comments.md
rm -f CRAN-SUBMISSION

# commit to release branch
git add --all .
git commit --allow-empty -m "Release $2"
git tag -a "v$2-release" -m "Release $2"

echo "Release branch created as release-$2. Tag on main is $2 and tag on release branch is v$2-release."
echo "Please verify before pushing with git push --tags"