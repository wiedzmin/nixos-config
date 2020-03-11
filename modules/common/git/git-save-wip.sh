REPO_ROOT=$1
if [[ ! -n $REPO_ROOT || ! -d $REPO_ROOT/.git ]]; then
    exit 1
fi

if [[ "$FORCE_STG" != "yes" && $(DISPLAY=:0 @xprintidleBinary@) -lt $((@gitIdletimeStgit@*1000)) ]]; then
    exit 0
fi

set -x

cd $REPO_ROOT

@stgitBinary@ init

@stgitBinary@ repair

if [[ ! -z $(@gitBinary@ status --porcelain) ]]; then
    @gitBinary@ add .
    PATCH_DESC="WIP $(date -R)"
    @stgitBinary@ new -m "$PATCH_DESC"
    @stgitBinary@ refresh
fi
