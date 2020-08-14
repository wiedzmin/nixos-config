ESSID=$(iwgetid -r)
if [ $? == 255 ]; then
  echo "No WiFi"
else
  STRENGTH=$(($(awk 'NR==3 {print substr($3, 1, length($3)-1)}' /proc/net/wireless) * 100 / 70))
  QUALITY_COLOR=
  case 1 in
  $((STRENGTH < 30)))
    QUALITY_COLOR=red
    ;;
  $((STRENGTH >= 30 && STRENGTH < 70)))
    QUALITY_COLOR=yellow
    ;;
  $((STRENGTH >= 70 && STRENGTH <= 100)))
    QUALITY_COLOR=green
    ;;
  esac
  echo $ESSID: "<fc=$QUALITY_COLOR>$STRENGTH</fc>%"
fi
