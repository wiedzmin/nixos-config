@maimBinary@ -o -i $(@xdotoolBinary@ getactivewindow) --format png /dev/stdout | \
    @teeBinary@ @screenshotsBasedir@/screenshot-$(date @screenshotsDateFormat@.png | @trBinary@ -d '[:cntrl:]') | \
    @xclipBinary@ -selection primary -t image/png -i
