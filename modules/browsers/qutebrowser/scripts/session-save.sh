SESSION_NAME=session-$(date +%Y-%m-%d-%H-%M-%S | tr -d '[:cntrl:]')
echo "{\"args\":[\":session-save --quiet $SESSION_NAME\", \":session-save --quiet\"], \"target_arg\":\"\", \"protocol_version\":1}" |
  socat - UNIX-CONNECT:"${XDG_RUNTIME_DIR}/qutebrowser/ipc-$(echo -n "$USER" | md5sum | cut -d' ' -f1)"
# TODO: parameterize minutes below
find "$HOME/.local/share/qutebrowser/sessions" -maxdepth 1 -type f -cmin +180 -exec rm -f {} ';'
