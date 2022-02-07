{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.shell.bookmarks;
  user = config.attributes.mainUser.name;
in {
  options = {
    shell.bookmarks = {
      enable = mkOption {
        type = types.bool;
        description = "Whether to enable shell bookmarks";
        default = false;
      };
      path = mkOption {
        type = types.str;
        description = "Where to store shell bookmarks, relative to $HOME";
        default = ".bookmarks";
      };
      order = mkOption {
        type = types.bool;
        default = false;
        description = "Keep order of bookmarks";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      assertions = [{
        assertion = config.shell.zsh.enable;
        message = "shell/bookmarks: enable Zsh first.";
      }];

      shell.core.variables = [({
        FZF_MARKS_FILE = "$HOME/${cfg.path}";
        FZF_MARKS_JUMP = "^[[1;5P";
      } // lib.optionalAttrs cfg.order { FZF_MARKS_KEEP_ORDER = "1"; })];

      home-manager.users."${user}" = {
        home.activation = {
          populateShellBookmarks = {
            after = [ ];
            before = [ "linkGeneration" ];
            data = ''
              echo "${localBookmarksKVText config.navigation.bookmarks.entries}" > ${
                homePrefix user cfg.path
              }'';
          };
        };
        programs.zsh = {
          plugins = [{
            name = "fzf-marks";
            file = "fzf-marks.plugin.zsh";
            src = pkgs.fetchFromGitHub {
              owner = "urbainvaes";
              repo = "fzf-marks";
              rev = "f2e8844ce813f8ad35a1903eb8c680c4492e153b";
              sha256 = "0a8jlwc12m0xid2v4d7rxzci91w8qrc4x91jq4lv0lm62v2w4n1j";
            };
          }];
        };
      };
    })
  ];
}
