set -e

devenv_data=$(<@projectEnvConfigName@)
devenv_data_filtered=$(echo "$devenv_data" | xargs -d '\n' find 2>/dev/null)
devenv_filelist=$(echo "$devenv_data_filtered" | tr '\n' ' ')

git reset # clean up index from unrelated staged changes
git add -- $devenv_filelist
git stash -m "dev-env" -- $devenv_filelist
