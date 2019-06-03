{config, pkgs, lib, ...}:
with import ../../const.nix {inherit config pkgs;};
{
    imports = [
        ../../private/common.nix
        ../../private/job.nix
    ];

    home-manager.users.alex3rd = {
        home.file = {
            ".config/wtf/config.yml".text = ''
                wtf:
                  colors:
                    border:
                      focusable: darkslateblue
                      focused: orange
                      normal: gray
                  grid:
                    columns: [40, 40]
                    rows: [13, 13, 4]
                  refreshInterval: 1
                  mods:
                    clocks:
                      colors:
                        rows:
                          even: "lightblue"
                          odd: "white"
                      enabled: true
                      locations:
                        ${builtins.concatStringsSep "\n        "
                            (lib.mapAttrsToList (caption: timezone: caption + ": " + timezone) config.common.worldClocks)}
                      position:
                        top: 0
                        left: 0
                        height: 1
                        width: 1
                      refreshInterval: 15
                      sort: "alphabetical"
                    security:
                      enabled: true
                      position:
                        top: 1
                        left: 0
                        height: 1
                        width: 1
                      refreshInterval: 3600
                    status:
                      enabled: true
                      position:
                        top: 2
                        left: 0
                        height: 1
                        width: 2
                      refreshInterval: 1
                    system:
                      enabled: true
                      position:
                        top: 0
                        left: 1
                        height: 1
                        width: 1
                      refreshInterval: 3600
                    textfile:
                      enabled: true
                      filePath: "~/.config/wtf/config.yml"
                      position:
                        top: 1
                        left: 1
                        height: 1
                        width: 1
                      refreshInterval: 30
            '';
        };
        programs.htop = {
            enable = true;
            fields = [
                "USER"
                "PRIORITY"
                "NICE"
                "M_SIZE"
                "STATE"
                "PERCENT_CPU"
                "PERCENT_MEM"
                "TIME"
                "COMM"
            ];
            meters.left = [ "AllCPUs" "Memory" ];
            colorScheme = 0;
            detailedCpuTime = true;
        };
        programs.command-not-found.enable = true;
        programs.lesspipe.enable = true;
        programs.man.enable = true;
        programs.info.enable = true;
        programs.skim = {
            enable = true;
            historyWidgetOptions = [
                "--exact"
            ];
            defaultOptions = [
                "--height 40%"
                "--prompt ⟫"
            ];
            fileWidgetCommand = "${pkgs.fd}/bin/fd --type f";
            fileWidgetOptions = [ "--preview 'head {}'" ];
            changeDirWidgetCommand = "${pkgs.fd}/bin/fd --type d";
            changeDirWidgetOptions = [ "--preview 'tree -C {} | head -200'" ];
            enableZshIntegration = true;
        };
        programs.direnv = {
            enable = true;
            enableZshIntegration = true;
        };
        programs.z-lua = {
            enable = true;
            enableZshIntegration = true;
            options = [
                "fzf"
                "enhanced"
                "once"
            ];
        };
    };
}
