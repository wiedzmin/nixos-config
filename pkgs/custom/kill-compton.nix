{ bash, procps, ... }:
''
    #!${bash}/bin/bash

    ${procps}/bin/pkill -f compton
''
