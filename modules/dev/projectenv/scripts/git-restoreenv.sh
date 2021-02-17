git_root=$(git rev-parse --show-toplevel)
envs_list=$(ls -a @projectEnvBackupRoot@ | grep $(basename $(dirname "$git_root"))_$(basename "$git_root") | sort -n -r)
envs_count=$(echo "$envs_list" | wc -l)
tip_env=$(echo "$envs_list" | head -n 1)
remainder_envs=$(echo "$envs_list" | tail -n +2)

if [[ "$@" =~ "--tidy" ]]; then
  for dev_env in $remainder_envs; do
    rm -rf "@projectEnvBackupRoot@/$dev_env"
  done
fi

if [[ ! "$@" =~ "--keep-devenv" ]]; then
  if [ "$envs_count" -gt 1 ]; then
    rm -rf @projectEnvBackupRoot@/$tip_env
  fi
fi

git-hideenv
git stash drop $(git stash list --max-count=1 --grep="dev-env" | cut -f1 -d":")
cp -a @projectEnvBackupRoot@/$tip_env/. .

devenv_data=$(<@projectEnvConfigName@)
devenv_data_filtered=$(echo "$devenv_data" | xargs -d '\n' find 2>/dev/null)
devenv_filelist=$(echo "$devenv_data_filtered" | tr '\n' ' ')
git reset # clean up index from unrelated staged changes
git add -- $devenv_filelist
