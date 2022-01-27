ESSID=$(iwgetid -r)
if [ $? == 255 ]; then
  echo "No WiFi"
else
  STRENGTH=$(($(awk 'NR==3 {print substr($3, 1, length($3)-1)}' /proc/net/wireless) * 100 / 70))
  QUALITY_COLOR=
  if ((STRENGTH < 30)); then
    QUALITY_COLOR=red
  fi
  if ((STRENGTH >= 30 && STRENGTH < 70)); then
    QUALITY_COLOR=yellow
  fi
  if ((STRENGTH >= 70 && STRENGTH <= 100)); then
    QUALITY_COLOR=green
  fi
  echo "$ESSID: <fc=$QUALITY_COLOR>$STRENGTH</fc>%"
fi
