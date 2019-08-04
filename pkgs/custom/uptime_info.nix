{ bash, dunst, gnused, procps, ... }:
''
    #!${bash}/bin/bash

    ${dunst}/bin/dunstify -t 7000 "Uptime: $(${procps}/bin/w | \
    ${gnused}/bin/sed -r '1 s/.*up *(.*),.*user.*/\1/g;q')"
''
