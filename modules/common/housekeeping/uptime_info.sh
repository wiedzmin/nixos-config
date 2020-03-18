dunstify -t 7000 "Uptime: $(w | sed -r '1 s/.*up *(.*),.*user.*/\1/g;q')"
