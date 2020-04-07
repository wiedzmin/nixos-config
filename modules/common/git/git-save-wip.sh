REPO_ROOT=$1
if [[ ! -n $REPO_ROOT || ! -d $REPO_ROOT/.git ]]; then
  exit 1
fi

if [[ "$FORCE_STG" != "yes" && $(DISPLAY=:0 xprintidle-ng) -lt $((@gitIdletimeStgit@ * 1000)) ]]; then
  exit 0
fi

set -x

cd $REPO_ROOT

stg init

stg repair

if [[ ! -z $(git status --porcelain) ]]; then
  git add .
  PATCH_DESC="WIP $(date -R)"
  stg new -m "$PATCH_DESC"
  stg refresh
fi
