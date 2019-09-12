{ bash, coreutils, docker, docker-machine, gawk, rofi, xsel, yad, ... }: ''
  #!${bash}/bin/bash

  declare -A CONTAINER_TRAITS

  CONTAINER_TRAITS=(
    ["name"]='{{index (split .Name "/") 1}}'
    ["created"]='{{.Created}}'
    ["path + args"]='{{.Path}} :: {{.Args}}'
    ["stats"]='{{println .State.Status}} {{.State.StartedAt}} <--> {{println .State.FinishedAt}} restarts: {{.RestartCount}}'
    ["ports"]='{{range $port, $mappings :=.NetworkSettings.Ports}}{{$port}} --> {{range $ifnum, $ifdef:=$mappings}}{{$ifnum}}) {{$ifdef.HostIp}}:{{$ifdef.HostPort}}{{end}}{{end}}'
    ["mounts"]='{{range $i, $mountpoint :=.Mounts}}{{with $mountpoint}}{{.Type}} {{.Destination}} --> {{.Source}} RW:{{.RW}}{{end}}{{end}}'
    ["env"]='{{range $entry :=.Config.Env}}{{with $entry}}{{println .}}{{end}}{{end}}'
    ["cmd"]='{{index .Config.Cmd 0}}'
    ["image"]='{{.Config.Image}}'
    ["volumes"]='{{range $vol, $data :=.Config.Volumes}}{{$vol}}: {{$data}}{{end}}'
    ["entrypoint"]='{{index .Config.Entrypoint 0}}'
    ["labels"]='{{range $name, $value :=.Config.Labels}}{{$name}}: {{println $value}}{{end}}'
    ["net: ip"]='{{range $network, $settings :=.NetworkSettings.Networks}}{{$settings.IPAddress}}{{end}}'
    ["net: gateway"]='{{range $network, $settings :=.NetworkSettings.Networks}}{{$settings.Gateway}}{{end}}'
    ["net: names"]='{{range $network, $settings :=.NetworkSettings.Networks}}{{$network}}/{{println $settings.Aliases}}{{end}}'
  )

  CONTAINER_STATUSES=(
    "alive"
    "all"
  )

  function show_list() {
      contents=("$@")
      for i in "''${contents[@]}";
      do
          echo "$i"
      done
  }

  function show_mapping_keys() {
      eval "declare -A contents="''${1#*=}
      for i in "''${!contents[@]}";
      do
          echo "$i"
      done
  }

  main() {
      HOST=$( cat /etc/hosts | ${gawk}/bin/awk '{print $2}' | ${coreutils}/bin/uniq | ${rofi}/bin/rofi -dmenu -p "Host" )
      if [ ! -z "$HOST" ]; then
          if [ "$HOST" == "localhost" ]; then
              eval $(${docker-machine}/bin/docker-machine env -u)
          else
              eval $(${docker-machine}/bin/docker-machine env $HOST)
          fi
          CONTAINER_STATUS=$( (show_list "''${CONTAINER_STATUSES[@]}") | ${rofi}/bin/rofi -dmenu -p "Status" )
          if [ -z "$CONTAINER_STATUS" ]; then
              exit 1
          fi
          if [ "$CONTAINER_STATUS" == "all" ]; then
              SELECTED_CONTAINER=$( ${docker}/bin/docker ps -a --format '{{.Names}}' | ${rofi}/bin/rofi -dmenu -p "Container" )
          else
              SELECTED_CONTAINER=$( ${docker}/bin/docker ps --format '{{.Names}}' | ${rofi}/bin/rofi -dmenu -p "Container" )
          fi
          if [ -n "$SELECTED_CONTAINER" ]; then
              SELECTED_TRAIT=$( (show_mapping_keys "$(declare -p CONTAINER_TRAITS)") | ${rofi}/bin/rofi -dmenu -p "Inspect" )
              if [ -n "$SELECTED_TRAIT" ]; then
                  INSPECT_COMMAND="${docker}/bin/docker inspect $SELECTED_CONTAINER --format='"''${CONTAINER_TRAITS[$SELECTED_TRAIT]}"'"
                  eval `echo $INSPECT_COMMAND` | tr -d '\n' | ${xsel}/bin/xsel -i --clipboard
                  eval `echo $INSPECT_COMMAND` > /tmp/docker_traits
                  ${yad}/bin/yad --filename /tmp/docker_traits --text-info
                  rm /tmp/docker_traits
              fi
          fi
      fi
  }

  main

  exit 0
''
