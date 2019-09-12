{ bash, ... }: ''
  #!${bash}/bin/bash

  CONTAINER_HANDLE=$1
  INPUT_PORT=''${2:-8000}
  TRACK_RESPONSES=''${3:-track}
  DUMPS_PATH=$HOME/goreplay_dumps
  DUMP_FILENAME="DUMPS_PATH/$CONTAINER_HANDLE-$(date +%Y-%m-%d-%T | tr -d '[:cntrl:]').dump"

  mkdir -p "$DUMPS_PATH"

  TRACK_RESPONSE_OPTION=""
  if [[ "$TRACK_RESPONSES" == "track" ]]; then
      TRACK_RESPONSE_OPTION="--input-raw-track-response"
  fi
  CONTAINER_PID=$(docker inspect --format '{{.State.Pid}}' "$CONTAINER_HANDLE")

  nsenter -t "$CONTAINER_PID" -n gor --input-raw ":$INPUT_PORT" --output-file "$DUMP_FILENAME" "$TRACK_RESPONSE_OPTION"
''
