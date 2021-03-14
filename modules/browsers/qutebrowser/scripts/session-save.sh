echo '{"args":[":session-save"], "target_arg":"", "protocol_version":1}' |
  socat - UNIX-CONNECT:"${XDG_RUNTIME_DIR}/qutebrowser/ipc-$(echo -n "$USER" | md5sum | cut -d' ' -f1)"
