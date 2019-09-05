{ bash, coreutils, maim, xclip, xdotool, yq-go, ... }:
''
  #!${bash}/bin/bash

  CONFIGFILE=''${1:-$HOME/.config/screenshots/screenshots.yml}
  SCREENSHOTS_PATH=$(${yq-go}/bin/yq r $CONFIGFILE screenshots.path)
  DATE_FORMAT=$(${yq-go}/bin/yq r $CONFIGFILE screenshots.date_format)
  ${maim}/bin/maim -o -i $(${xdotool}/bin/xdotool getactivewindow) --format png /dev/stdout | \
      ${coreutils}/bin/tee $SCREENSHOTS_PATH/screenshot-$(date $DATE_FORMAT.png | ${coreutils}/bin/tr -d '[:cntrl:]') | \
      ${xclip}/bin/xclip -selection primary -t image/png -i
''
