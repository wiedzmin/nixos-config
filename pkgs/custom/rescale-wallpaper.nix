{ bash, config, feh, ... }:
''
    #!${bash}/bin/bash

    ${feh}/bin/feh --bg-fill ${config.sys.wallpaper.baseDir}/${config.sys.wallpaper.current}
''
