set -e

devenv_data=$(<@projectEnvConfigName@)
devenv_data_filtered=$(echo "$devenv_data" | xargs -d '\n' find 2>/dev/null)
devenv_filelist=$(echo "$devenv_data_filtered" | tr '\n' ' ')
git_root=$(git rev-parse --show-toplevel)
ts_suffix=$(date @projectEnvBackupDateFormat@)
dump_dir=$(basename $(dirname "$git_root"))_$(basename "$git_root")_$ts_suffix

mkdir -p @projectEnvBackupRoot@/$dump_dir
cp -t @projectEnvBackupRoot@/$dump_dir $devenv_filelist
