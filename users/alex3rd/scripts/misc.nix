{config, pkgs, lib, ...}:

let
    bookshelfPath = "${config.users.extraUsers.alex3rd.home}/bookshelf";
    bookReaderUsePdftools = true;
    sedPlaceholderChar = "_";
    # TODO: generalize and find way to extract to module/lib
    prettifyValue = value:
       if builtins.typeOf value == "int" then
          builtins.toString value
       else if builtins.typeOf value == "bool" then
          if value == true then "✓"
          else "✗"
       else builtins.toString value;
    setToBashKeyValue = set: keyname: valueSep: omitKey:
        let
            keyValue = set.${keyname};
            strippedSet = builtins.removeAttrs set [keyname];
        in
            "[\"" + keyValue + "\"]=\"" +
                  (builtins.concatStringsSep valueSep
                            (pkgs.stdenv.lib.mapAttrsToList
                                  (key: value: if omitKey then "${prettifyValue value}"
                                                          else "${key}:${sedPlaceholderChar}${prettifyValue value}")
                                  strippedSet)) + "\"";
    unfoldListOfSetsByAttr = list: attr:
        let
            v = if builtins.length list == 0 then {${attr} = [];} else builtins.head list;
        in
        (map (elem: v // {${attr} = elem;} ) v.${attr}) ++
             (if builtins.length list == 0 then [] else (unfoldListOfSetsByAttr (builtins.tail list) attr));
    listOfSetsToShellHashtable = list: keyname: tablename: omitKey:
        "declare -A ${tablename}" + "\n" +
        "${tablename}=(" + "\n" +
            (builtins.concatStringsSep
                "\n" (map (attrs: setToBashKeyValue attrs keyname " " omitKey) list))
        +"\n" + ")";
in
{
    config = {
        nixpkgs.config.packageOverrides = super: {
            rofi_bookshelf = pkgs.writeShellScriptBin "rofi_bookshelf" ''
                IFS=$'\n'
                BOOKS=$(${pkgs.findutils}/bin/find ${bookshelfPath} -name "*.pdf"${ if !bookReaderUsePdftools then
                        " -o -name \"*.djvu\"" else ""})

                list_books() {
                    for i in "''${BOOKS[@]}"
                    do
                        echo "$i"
                    done
                }

                main() {
                    SELECTED_BOOK=$( (list_books) | ${pkgs.rofi}/bin/rofi -dmenu -p "EBook " )
                    if [ -n "$SELECTED_BOOK" ]; then
                    ${if bookReaderUsePdftools then ''
                        ${pkgs.emacs}/bin/emacsclient --eval "(find-file \"$SELECTED_BOOK\")" >& /dev/null
                        sleep 0.5
                        ${pkgs.wmctrl}/bin/wmctrl -l -x | grep -E "emacs\.Emacs.+(pdf|djvu)$" | ${pkgs.gawk}/bin/awk '{print $1}' | \
                                                       ${pkgs.findutils}/bin/xargs ${pkgs.wmctrl}/bin/wmctrl -i -a
                    '' else ''
                        ${pkgs.zathura}/bin/zathura "$SELECTED_BOOK" & >& /dev/null
                    ''}
                    fi
                }

                main

                exit 0
            '';
            rofi_insert_snippet = pkgs.writeShellScriptBin "rofi_insert_snippet" ''
                ask_for_snippets() {
                    SNIPPETS=$(<${config.users.extraUsers.alex3rd.home}/${config.common.snippets.file})
                    for i in "''${SNIPPETS[@]}"
                    do
                        echo "$i"
                    done
                }

                main() {
                    SNIPPET=$( (ask_for_snippets) | ${pkgs.rofi}/bin/rofi -dmenu -p "Snippet" )
                    if [ ! -n "$SNIPPET" ]; then
                        exit 1
                    fi
                    echo $SNIPPET | tr -d '\n' | ${pkgs.xsel}/bin/xsel -i --clipboard
                    ${pkgs.xdotool}/bin/xdotool type -- "$(${pkgs.xsel}/bin/xsel -bo)"
                }

                main

                exit 0
            '';
            # TODO: think of free-form option(s)
            rofi_dbms = pkgs.writeShellScriptBin "rofi_dbms" ''
                declare -A DBMS_TRAITS

                DBMS_TRAITS=(
                ${builtins.concatStringsSep "\n"
                  (pkgs.stdenv.lib.mapAttrsToList
                      (ip: meta: "  [\"${meta.alias}\"]=\"${ip} ${meta.command} ${meta.user} ${meta.passwordPassPath}\"")
                        (config.job.dbmsTraits))}
                )

                MYCLI_BINARY=${pkgs.mycli}/bin/mycli
                PGCLI_BINARY=${pkgs.pgcli}/bin/pgcli

                list_dbms_traits() {
                    for i in "''${!DBMS_TRAITS[@]}"
                    do
                        echo "$i"
                    done
                }

                main() {
                    DBMS_META="''${DBMS_TRAITS[$( (list_dbms_traits) | ${pkgs.rofi}/bin/rofi -dmenu -p "Connect" )]}"
                    if [ -n "$DBMS_META" ]; then
                        DBMS_IP=$(echo $DBMS_META | ${pkgs.coreutils}/bin/cut -f1 -d\ )
                        DBMS_COMMAND=$(echo $DBMS_META | ${pkgs.coreutils}/bin/cut -f2 -d\ )
                        DBMS_USER=$(echo $DBMS_META | ${pkgs.coreutils}/bin/cut -f3 -d\ )
                        DBMS_PASSWORD_PASS_PATH=$(echo $DBMS_META | ${pkgs.coreutils}/bin/cut -f4 -d\ )
                        DBMS_PASSWORD=$(${pkgs.pass}/bin/pass $DBMS_PASSWORD_PASS_PATH)
                        CLI_BINARY_VARNAME="''${DBMS_COMMAND^^}_BINARY"
                        CLI_EXECUTABLE="''${!CLI_BINARY_VARNAME}"
                        CLI_EXECUTABLE="''${!CLI_BINARY_VARNAME}"
                        ${pkgs.tmux}/bin/tmux new-window "$CLI_EXECUTABLE --host $DBMS_IP --user $DBMS_USER --password $DBMS_PASSWORD"
                    fi
                }

                main

                exit 0
            '';
            rofi_entrypoint = pkgs.writeShellScriptBin "rofi_entrypoint" ''
                declare -A ROFI_SCRIPTS

                # TODO: try to make more declarative
                ROFI_SCRIPTS=(
                  ["rofi_bookshelf"]="${pkgs.rofi_bookshelf}/bin/rofi_bookshelf"
                  ["rofi_ssh_custom_user"]="${pkgs.rofi_ssh_custom_user}/bin/rofi_ssh_custom_user"
                  ["rofi_webjumps"]="${pkgs.rofi_webjumps}/bin/rofi_webjumps"
                  ["rofi_searchengines_prompt"]="${pkgs.rofi_searchengines_prompt}/bin/rofi_searchengines_prompt"
                  ["rofi_searchengines_selection"]="${pkgs.rofi_searchengines_selection}/bin/rofi_searchengines_selection"
                  ["rofi_extra_hosts_traits"]="${pkgs.rofi_extra_hosts_traits}/bin/rofi_extra_hosts_traits"
                  ["rofi_insert_snippet"]="${pkgs.rofi_insert_snippet}/bin/rofi_insert_snippet"
                  ["rofi_docker_stacks_info"]="${pkgs.rofi_docker_stacks_info}/bin/rofi_docker_stacks_info"
                  ["rofi_remote_docker_logs"]="${pkgs.rofi_remote_docker_logs}/bin/rofi_remote_docker_logs"
                  ["rofi_dbms"]="${pkgs.rofi_dbms}/bin/rofi_dbms"
                  ["rofi_containerized_services_discovery"]="${pkgs.rofi_containerized_services_discovery}/bin/rofi_containerized_services_discovery"
                  ["rofi_ctop"]="${pkgs.rofi_ctop}/bin/rofi_ctop"
                  ["rofi_jnettop"]="${pkgs.rofi_jnettop}/bin/rofi_jnettop"
                )

                list_rofi_scripts() {
                    for i in "''${!ROFI_SCRIPTS[@]}"
                    do
                        echo "$i"
                    done
                }

                main() {
                    ROFI_SCRIPT="''${ROFI_SCRIPTS[$( (list_rofi_scripts) | ${pkgs.rofi}/bin/rofi -dmenu -p "Script" )]}"
                    if [ -n "$ROFI_SCRIPT" ]; then
                        $ROFI_SCRIPT
                    fi
                }

                main

                exit 0
            '';
        };
    };
}
