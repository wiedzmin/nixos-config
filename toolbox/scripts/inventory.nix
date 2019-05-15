{config, pkgs, lib, ...}:

{
    config = {
        nixpkgs.config.packageOverrides = super: {
            rofi_autorandr_profiles = pkgs.writeShellScriptBin "rofi_autorandr_profiles" ''
                AUTORANDR_PROFILES_PATH=''${1:-$HOME/.config/autorandr}

                AUTORANDR_PROFILES=(
                $(${pkgs.fd}/bin/fd --type d . $AUTORANDR_PROFILES_PATH -x echo '{/}' | ${pkgs.gnugrep}/bin/grep -ve "\.d")
                )

                list_autorandr_profiles() {
                    for i in "''${AUTORANDR_PROFILES[@]}"
                    do
                        echo "$i"
                    done
                }

                main() {
                    SELECTED_PROFILE=$( (list_autorandr_profiles) | ${pkgs.rofi}/bin/rofi -dmenu -p "Profile " )
                    if [ -n "$SELECTED_PROFILE" ]; then
                        ${pkgs.autorandr}/bin/autorandr --load "$SELECTED_PROFILE" & >& /dev/null
                    fi
                }

                main

                exit 0
            '';
            rofi_tmuxp_sessions = pkgs.writeShellScriptBin "rofi_tmuxp_sessions" ''
                TMUXP_SESSIONS_PATH=''${1:-$HOME/tmuxp}

                TMUXP_SESSIONS=(
                $(${pkgs.fd}/bin/fd --maxdepth 1 --type l '.yml' $TMUXP_SESSIONS_PATH -x echo '{/.}')
                )

                list_tmuxp_sessions() {
                    for i in "''${TMUXP_SESSIONS[@]}"
                    do
                        echo "$i"
                    done
                }

                main() {
                    SELECTED_SESSION=$( (list_tmuxp_sessions) | ${pkgs.rofi}/bin/rofi -dmenu -p "Profile " )
                    if [ -n "$SELECTED_SESSION" ]; then
                        ${pkgs.tmuxp}/bin/tmuxp load -y -d $TMUXP_SESSIONS_PATH/$SELECTED_SESSION.yml >/dev/null 2>&1 &
                    fi
                }

                main

                exit 0
            '';
            rofi_service_journal = pkgs.writeShellScriptBin "rofi_service_journal" ''
                SERVICE_CONTEXTS=(
                  "system"
                  "user"
                )

                ask_for_context() {
                    for i in "''${SERVICE_CONTEXTS[@]}"
                    do
                        echo "$i"
                    done
                }

                main() {
                    CONTEXT=$( (ask_for_context) | ${pkgs.rofi}/bin/rofi -dmenu -p "Context" )
                    if [ ! -n "$CONTEXT" ]; then
                        exit 1
                    fi
                    SERVICE=$(${pkgs.systemd}/bin/systemctl $([[ "$CONTEXT" == "user" ]] && echo --user) list-unit-files | \
                              grep -v target | ${pkgs.gawk}/bin/awk '{print $1}' | \
                              ${pkgs.rofi}/bin/rofi -dmenu -p "Service")
                    if [ -n "$SERVICE" ]; then
                        ${pkgs.tmux}/bin/tmux new-window "${pkgs.systemd}/bin/journalctl $([[ "$CONTEXT" == "user" ]] && echo --user) -u $SERVICE"
                    fi
                }

                main

                exit 0
            '';
        };
    };
}
