{config, pkgs, ...}:

let
    bookshelfPath = "${config.users.extraUsers.alex3rd.home}/bookshelf";
    autorandrProfilesPath = "${config.users.extraUsers.alex3rd.home}/.config/autorandr";
    tmuxpSessionsPath = "${config.users.extraUsers.alex3rd.home}/tmuxp";
    bookReaderUsePdftools = true;
    currentUser = "alex3rd";
    previousUser = "octocat";
    screenshotDateFormat = "%Y-%m-%d-%T";
in
{
    config = {
        nixpkgs.config.packageOverrides = super: {
            rescale-wallpaper = pkgs.writeShellScriptBin "rescale-wallpaper" ''
                ${pkgs.feh}/bin/feh --bg-fill ${config.x11.wallpapers_dir}/${config.x11.current_wallpaper}
            '';
            shell-capture = pkgs.writeShellScriptBin "shell-capture" ''
                TEMPLATE="$1"
                if [[ ! -n $TEMPLATE ]]
                then
                    exit 1
                fi
                TITLE="$*"
                if [[ -n $TMUX ]]
                then
                    TITLE=$(tmux display-message -p '#S')
                    tmux send -X copy-pipe-and-cancel "xclip -i -selection primary"
                fi

                if [[ -n $TITLE ]]
                then
                    emacsclient -n "org-protocol://capture?template=$TEMPLATE&title=$TITLE"
                else
                    emacsclient -n "org-protocol://capture?template=$TEMPLATE"
                fi
            '';
            pass_curl_helper = pkgs.writeShellScriptBin "pass_curl_helper" ''
                PASS_ENTRY=$1
                echo $(${pkgs.pass}/bin/pass $PASS_ENTRY | ${pkgs.coreutils}/bin/tr '\n' ' ' | \
                     ${pkgs.gawk}/bin/awk '{print $3 ":" $1}')
            '';
            tar_encrypt = pkgs.writeShellScriptBin "tar_encrypt" ''
                # encrypts files/directory with default key identity to tar stream

                IN=$1
                if [[ -z $IN ]]; then
                    echo "no input file provided, exiting"
                    exit 1
                fi
                SOURCE_BASENAME=''${IN%.*}
                ${pkgs.gnupg}/bin/gpgtar -r "${config.common.primaryGpgKeyID}" -u "${config.common.primaryGpgKeyID}" \
                                         -e $IN > $SOURCE_BASENAME.tar.gpg
            '';
            tar_decrypt = pkgs.writeShellScriptBin "tar_decrypt" ''
                # decrypts files/directory from tar stream with default key identity

                IN=$1
                if [[ -z $IN ]]; then
                    echo "no input file provided, exiting"
                    exit 1
                fi
                WD=`pwd`
                SOURCE_BASENAME=''${IN%.*}
                ${pkgs.gnupg}/bin/gpg --output $WD/$SOURCE_BASENAME_decrypted.tar --decrypt $IN && \
                                        tar xf $WD/$SOURCE_BASENAME_decrypted.tar && \
                                        rm $WD/$SOURCE_BASENAME_decrypted.tar
            '';
            watch_dunst = pkgs.writeShellScriptBin "watch_dunst" ''
                FORCE=$1
                DUNST_COUNT=$(${pkgs.procps}/bin/pgrep -x -c .dunst-wrapped)
                if [ $DUNST_COUNT -gt 1 ]; then
                    ${pkgs.logger}/bin/logger "Too much dunsts ($DUNST_COUNT), shooting down..."
                    pkill -x -KILL .dunst-wrapped
                    exit 0
                elif [ "$FORCE" == "force" ]; then
                    pkill -x -KILL .dunst-wrapped
                    exit 0
                fi
            '';
            screenshot_active_window = pkgs.writeShellScriptBin "screenshot_active_window" ''
                ${pkgs.maim}/bin/maim -o -i $(xdotool getactivewindow) --format png /dev/stdout | \
                    tee " ++ Profile.screenshotsPath ++ "/screenshot-$(date +\"${screenshotDateFormat}\" | \
                    tr -d '[:cntrl:]') | ${pkgs.xclip}/bin/xclip -selection primary -t image/png -i
            '';
            screenshot_region = pkgs.writeShellScriptBin "screenshot_region" ''
                ${pkgs.maim}/bin/maim -o -s --format png /dev/stdout | \
                    tee " ++ Profile.screenshotsPath ++ "/screenshot-$(date +\"${screenshotDateFormat}\" | \
                    tr -d '[:cntrl:]') | ${pkgs.xclip}/bin/xclip -selection primary -t image/png -i
            '';
            screenshot_full = pkgs.writeShellScriptBin "screenshot_full" ''
                ${pkgs.maim}/bin/maim -o --format png /dev/stdout | \
                    tee " ++ Profile.screenshotsPath ++ "/screenshot-$(date +\"${screenshotDateFormat}\" | \
                    tr -d '[:cntrl:]') | ${pkgs.xclip}/bin/xclip -selection primary -t image/png -i
            '';
       };
    };
}
