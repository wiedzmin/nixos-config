ls -a @firefoxSessionsPath@ | grep @firefoxSessionsNameTemplate@ |
  sort -n -r | (
  i=0
  while read -r f; do
    if [ $i -lt @firefoxSessionsHistoryLength@ ]; then
      ((i++))
      continue
    else
      rm -f "@firefoxSessionsPath@/$f"
    fi
  done
)
