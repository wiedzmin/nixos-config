{ bash, buku, coreutils, dunst, gawk, rofi, xsel, ... }: ''
  #!${bash}/bin/bash

  _rofi () {
      ${rofi}/bin/rofi -dmenu -i -no-levenshtein-sort -width 1000 "$@"
  }

  is_url () {
      url_regex='(https?|ftp|file)://[-A-Za-z0-9\+&@#/%?=~_|!:,.;]*[-A-Za-z0-9\+&@#/%=~_|]'
      url_candidate=$1
      if [[ $url_candidate =~ $url_regex ]]
      then
          return 0
      fi
      return 1
  }

  collect_tags () {
      taglist=()
      sep=''${1:-,}
      tagcloud=$(${buku}/bin/buku --np --st | \
                 ${gawk}/bin/awk '{$NF=""; print $0}' | \
                 ${coreutils}/bin/cut -d ' ' -f2 | sort -u )
      while true; do
          tag=$(echo $tagcloud | tr ' ' '\n' | _rofi -p '> ' -mesg "Add tag" -custom)
          keep_going=$?
          if [[ $keep_going -ne 0 ]]; then
              break
          fi
          tag=$(echo "$tag" | tr -d '[:space:]')
          taglist+=("$tag$sep")
          tagcloud=( "''${tagcloud[@]/$tag}" )
      done
  }

  sleep_sec=''${1:-1}

  add_mark () {
      inserturl=$(echo -e "$(${xsel}/bin/xsel -o -b)" | _rofi -p '> ' -mesg "Use URL below or type manually")
      if [[ $? -ne 0 ]]; then
          exit
      fi
      is_url $inserturl
      if [[ $? -ne 0 ]]; then
          ${dunst}/bin/dunstify -t 5000 -u critical "URL is not valid, exiting"
          exit
      fi

      add_tags
  }

  add_tags () {
      collect_tags ","
      if [[ $(echo "''${taglist}" | wc -l) -gt 0 ]]; then
          ${buku}/bin/buku -a ''${inserturl} ''${taglist[@]}
      else
          ${buku}/bin/buku -a ''${inserturl}
      fi
  }

  main() {
      sleep $sleep_sec
      add_mark
      ${dunst}/bin/dunstify -t 5000 "Bookmark added: $inserturl"
  }

  main

  exit 0
''
