{ bash, config, docker, docker-machine, pkgs, rofi, ... }:
with import ../../pkgs/const.nix { inherit config pkgs; }; ''
  #!${bash}/bin/bash

  # TODO: think how to restrict networks/ports output (maybe pick first ones)
  main() {
      eval $(${docker-machine}/bin/docker-machine env -u) # ensure we cosidering only local containers
      SELECTED_CONTAINER=$( ${docker}/bin/docker ps --format '{{.Names}}' | ${rofi}/bin/rofi -dmenu -p "Container" )
      if [ ! -z "$SELECTED_CONTAINER" ]; then
          CONTAINER_IP=$(${docker}/bin/docker inspect $SELECTED_CONTAINER --format='{{range $network, $settings :=.NetworkSettings.Networks}}{{$settings.IPAddress}}{{end}}')
          EXPOSED_PORT=$(${docker}/bin/docker inspect $SELECTED_CONTAINER --format='{{range $port, $mappings :=.NetworkSettings.Ports}}{{$port}}{{end}}' | ${coreutils}/bin/cut -f1 -d/)
          ${firefoxOpenPageCmd} http://$CONTAINER_IP:$EXPOSED_PORT
      fi
  }

  main

  exit 0
''
