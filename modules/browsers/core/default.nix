{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.browsers.core;
  user = config.attributes.mainUser.name;
  hm = config.home-manager.users.${user};
  nurpkgs = pkgs.unstable.nur.repos.wiedzmin;
  prefix = config.wmCommon.prefix;
in {
  options = {
    browsers.core = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable navigation infra.
        '';
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable customized navigation for Emacs.
        '';
      };
      wm.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable WM keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      assertions = [
        {
          assertion = config.ext.networking.vpn.enable;
          message = "browsers/core: must enable vpn functionality.";
        }
        {
          assertion = config.workstation.systemtraits.enable;
          message = "browsers/core: must enable systemtraits maintainence.";
        }
      ];

      nixpkgs.config.packageOverrides = _: rec {
        search_prompt = mkPythonScriptWithDeps "search_prompt" (with pkgs; [ nurpkgs.pystdlib python3Packages.redis ])
          (readSubstituted ../subst.nix ./scripts/search_prompt.py);
        search_selection =
          mkPythonScriptWithDeps "search_selection" (with pkgs; [ nurpkgs.pystdlib python3Packages.redis xsel ])
          (readSubstituted ../subst.nix ./scripts/search_selection.py);
        webjumps = mkPythonScriptWithDeps "webjumps" (with pkgs; [ nurpkgs.pystdlib python3Packages.redis vpnctl xsel ])
          (readSubstituted ../subst.nix ./scripts/webjumps.py);
      };

      workstation.systemtraits.instructions = with config.navigation.bookmarks; ''
        ${pkgs.redis}/bin/redis-cli set nav/webjumps ${
          lib.strings.escapeNixString
          (builtins.toJSON (remoteWebjumps (enabledRemotes entries) separator.fields separator.tags))
        }
        ${pkgs.redis}/bin/redis-cli set nav/searchengines ${
          lib.strings.escapeNixString (builtins.toJSON (remoteSearchEngines (enabledRemotes entries)
            separator.fields separator.tags))
        }
      '';

      pim.timetracking.rules = with config.attributes.browser; ''
        -- TODO: parameterize web resources
        current window $program == [${
          concatStringListsQuoted ", " [ default.windowClass fallback.windowClass ]
        }] ==> tag activity:web,
        current window ($program == [${
          concatStringListsQuoted ", " [ default.windowClass fallback.windowClass ]
        }] && $title =~ /Gmail/) ==> tag web:Gmail,
        current window ($program == [${
          concatStringListsQuoted ", " [ default.windowClass fallback.windowClass ]
        }] && $title =~ /Google/) ==> tag web:Google,
        current window ($program == [${
          concatStringListsQuoted ", " [ default.windowClass fallback.windowClass ]
        }] && $title =~ /wikipedia/) ==> tag site:wikipedia,
      '';
    })
    (mkIf (cfg.enable && cfg.emacs.enable) {
      ide.emacs.core.extraPackages = epkgs: [ epkgs.atomic-chrome ];
      ide.emacs.core.config = readSubstituted ../../subst.nix ./emacs/browsers.el;
    })
    (mkIf (cfg.enable && cfg.wm.enable) {
      wmCommon.keys = [
        {
          key = [ prefix "slash" ];
          cmd = "${pkgs.search_selection}/bin/search_selection";
          mode = "root";
        }
        {
          key = [ prefix "Shift" "slash" ];
          cmd = "${pkgs.search_selection}/bin/search_selection --fallback";
          mode = "root";
        }
        {
          key = [ prefix "Control" "slash" ];
          cmd = "${pkgs.search_prompt}/bin/search_prompt";
          mode = "root";
        }
        {
          key = [ prefix "Control" "Shift" "slash" ];
          cmd = "${pkgs.search_prompt}/bin/search_prompt --fallback";
          mode = "root";
        }
        {
          key = [ prefix "j" ];
          cmd = "${pkgs.webjumps}/bin/webjumps";
          mode = "root";
        }
        {
          key = [ prefix "Shift" "j" ];
          cmd = "${pkgs.webjumps}/bin/webjumps --fallback";
          mode = "root";
        }
        {
          key = [ prefix "Control" "j" ];
          cmd = "${pkgs.webjumps}/bin/webjumps --copy";
          mode = "root";
        }
      ];
    })
    (mkIf (cfg.enable && config.attributes.debug.scripts) {
      home-manager.users.${user} = {
        home.packages = with pkgs; [ search_prompt search_selection webjumps ];
      };
    })
  ];
}
