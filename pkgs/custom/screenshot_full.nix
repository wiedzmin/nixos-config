{ bash, coreutils, maim, shyaml, xclip, ... }:
''
    #!${bash}/bin/bash

    CONFIGFILE=''${1:-$HOME/.config/screenshots/screenshots.yml}
    SCREENSHOTS_PATH=$(${shyaml}/bin/shyaml -gy screenshots.path $CONFIGFILE)
    DATE_FORMAT=$(${shyaml}/bin/shyaml -gy screenshots.date_format $CONFIGFILE)
    ${maim}/bin/maim -o --format png /dev/stdout | \
        ${coreutils}/bin/tee $SCREENSHOTS_PATH/screenshot-$(date $DATE_FORMAT.png | ${coreutils}/bin/tr -d '[:cntrl:]') | \
        ${xclip}/bin/xclip -selection primary -t image/png -i
''
