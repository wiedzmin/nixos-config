{config, pkgs, lib, ...}:

let
    bookshelfPath = "${config.users.extraUsers.alex3rd.home}/bookshelf";
    bookReaderUsePdftools = true;
    currentUser = "alex3rd";
    previousUser = "octocat";
    screenshotDateFormat = "%Y-%m-%d-%T";
    dockerStackPsCustomFormat = "{{.Name}}   {{.Image}}   {{.Node}} {{.DesiredState}}   {{.CurrentState}}";
    useDockerStackPsCustomFormat = false;
    dockerStackShowOnlyRunning = true;
    sedPlaceholderChar = "_";
    firefoxOpenPageCmd = "${pkgs.firefox-bin}/bin/firefox --new-window";
    chromiumOpenPageCmd = "${pkgs.chromium}/bin/chromium";
in
{
    config = {
        nixpkgs.config.packageOverrides = super: {
            rofi_autorandr_profiles = pkgs.writeShellScriptBin "rofi_autorandr_profiles" ''
                AUTORANDR_PROFILES_PATH=''${1:-$HOME/.config/autorandr}

                AUTORANDR_PROFILES=(
                $(${pkgs.findutils}/bin/find $AUTORANDR_PROFILES_PATH -mindepth 1 -maxdepth 1 -type d -exec basename {} \;)
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
                $(${pkgs.findutils}/bin/find $TMUXP_SESSIONS_PATH -mindepth 1 -maxdepth 1 -type l -exec basename {} .yml \;)
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
