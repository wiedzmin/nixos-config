@dunstifyBinary@ -t 7000 "Uptime: $(@wBinary@ | \
    @sedBinary@ -r '1 s/.*up *(.*),.*user.*/\1/g;q')"
