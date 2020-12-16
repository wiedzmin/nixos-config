declare -A REGEXP_TO_DATECMD
REGEXP_TO_DATECMD=(
  ["screenshot-[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}_[0-9]\{2\}\:[0-9]\{2\}\:[0-9]\{2\}"]='echo ''${FILENAME:11:4} ''${FILENAME:16:2} ''${FILENAME:19:2}'
  ["screenshot-[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}_[0-9]\{2\}-[0-9]\{2\}-[0-9]\{2\}"]='echo ''${FILENAME:11:4} ''${FILENAME:16:2} ''${FILENAME:19:2}'
  ["[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}_[0-9]\{2\}\:[0-9]\{2\}\:[0-9]\{2\}_[0-9]\+x[0-9]\+_scrot"]='echo ''${FILENAME:0:4} ''${FILENAME:5:2} ''${FILENAME:8:2}'
  ["[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}-[0-9]\{6\}_[0-9]\+x[0-9]\+_scrot"]='echo ''${FILENAME:0:4} ''${FILENAME:5:2} ''${FILENAME:8:2}'
  ["screenshot-[0-9]\{2\}-[0-9]\{2\}-[0-9]\{4\}-[0-9]\{2\}\:[0-9]\{2\}\:[0-9]\{2\}"]='echo ''${FILENAME:17:4} ''${FILENAME:14:2} ''${FILENAME:11:2}'
  ["screenshot-[0-9]\{2\}\:[0-9]\{2\}\:[0-9]\{2\}\\ [0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}"]='echo ''${FILENAME:20:4} ''${FILENAME:25:2} ''${FILENAME:28:2}'
  ["screenshot-[0-9]\{2\}\:[0-9]\{2\}\:[0-9]\{2\}_[0-9]\{4\}-[0-9]\{2\}-[0-9]\{2\}"]='echo ''${FILENAME:20:4} ''${FILENAME:25:2} ''${FILENAME:28:2}'
)

for regexp in "''${!REGEXP_TO_DATECMD[@]}"; do
  FILELIST=$(ls -a @screenshotsBasedir@ | grep -e $regexp)
  DATECMD=''${REGEXP_TO_DATECMD[$regexp]}
  for FILENAME in $FILELIST; do
    DATE_PART=($(eval $DATECMD))
    YEAR=''${DATE_PART[0]}
    MONTH=''${DATE_PART[1]}
    DAY=''${DATE_PART[2]}
    DEST_PATH=@screenshotsBasedir@/$YEAR/$MONTH/$DAY
    mkdir -p @screenshotsBasedir@/$YEAR/$MONTH/$DAY
    echo "moving $FILENAME to $DEST_PATH"
    mv @screenshotsBasedir@/$FILENAME @screenshotsBasedir@/$YEAR/$MONTH/$DAY
  done
done

exit 0
