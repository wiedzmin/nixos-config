maim -o -i $(xdotool getactivewindow) --format png /dev/stdout |
  tee @screenshotsBasedir@/screenshot-$(date @screenshotsDateFormat@.png | tr -d '[:cntrl:]') |
  xclip -selection primary -t image/png -i
