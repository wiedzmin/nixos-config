{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.wm.xmonad;
  user = config.attributes.mainUser.name;
  yaml = pkgs.formats.yaml { };
in
{
  options = {
    wm.xmonad = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable xmonad.";
      };
      isDefault = mkOption {
        type = types.bool;
        default = false;
        description = "Set `XMonad` as default WM";
      };
      # FIXME: adopt new workspace keybindings implementation (presumably broken)
      internalKeys = mkOption {
        type = types.attrs;
        default = {
          "C-\\\\" = "sendMessage (XkbToggle Nothing)";
          "M-<Home>" = "toggleWS";
          "M-<Return>" = "promote";
          "M-<Space>" = "sendMessage NextLayout";
          "M-<Tab>" = "windows W.focusDown";
          "M-S-<Space>" = "setLayout $ XMonad.layoutHook conf";
          "M-S-<Tab>" = "windows W.focusUp";
          "M-<F12>" = "kill1";
          "M-S-j" = "windows W.swapDown";
          "M-S-k" = "windows W.swapUp";
          "M-S-q" = "io (exitWith ExitSuccess)";
          "M-h" = "sendMessage Shrink";
          "M-l" = "sendMessage Expand";
          "M-t" = "withFocused $ windows . W.sink";
          "M-x a" = "windows copyToAll"; # @@ Make focused window always visible
          "M-x k" = "killAllOtherCopies"; # @@ Toggle window state back
          "M-a 1" = ''namedScratchpadAction scratchpads "htop"'';
          "M-a 2" = ''namedScratchpadAction scratchpads "iotop"'';
          "M-a 3" = ''namedScratchpadAction scratchpads "gotop"'';
          "M-a 4" = ''namedScratchpadAction scratchpads "bc"'';
          "M-a 5" = ''namedScratchpadAction scratchpads "redis"'';
          # -- TODO: recall and add "multiple app windows"-aware raising
          "M-w <Backspace>" = "nextMatch History (return True)";
          "M-S-w M-S-n" = "moveTo Next EmptyWS"; # find a free workspace
          "M-S-w M-S-<Up>" = "shiftToPrev";
          "M-S-w M-S-<Down>" = "shiftToNext";
          "M-S-w M-S-<Right>" = "shiftToNext >> nextWS";
          "M-S-w M-S-<Left>" = "shiftToPrev >> prevWS";
          "M-S-w <Up>" = "prevWS";
          "M-S-w <Down>" = "nextWS";
          "M-w M-s" = "sinkAll";
          "M-w r" = "refresh";
          "M-w M-w" = "placeWorkplaces";
          "M-S-." = "placeFocused placePolicy";
          "M-<Right>" = "windowGo R True";
          "M-<Left>" = "windowGo L True";
          "M-<Up>" = "windowGo U True";
          "M-<Down>" = "windowGo D True";
        };
        description = "Internal (quite tightly coupled with) XMonad keybindings.";
      };
      bookmarks.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Whether to enable XMonad-related bookmarks";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      ide.emacs.core.extraPackages = epkgs: [ epkgs.haskell-mode ];

      environment.systemPackages = with pkgs; [ haskellPackages.xmobar ];
      home-manager.users."${user}" = {
        home.file = {
          ".xmonad/lib/XMonad/Util/ExtraCombinators.hs".source = ./assets/lib/ExtraCombinators.hs;
          ".xmonad/lib/XMonad/Util/WindowTypes.hs".source = ./assets/lib/WindowTypes.hs;
          ".xmonad/lib/XMonad/Util/Xkb.hs".source = ./assets/lib/XkbToggle.hs;
          ".xmonad/lib/XMonad/Workspaces.hs".text = readSubstituted config inputs pkgs [ ./subst.nix ] [ ./assets/lib/Workspaces.hs ];
          ".xmonad/xmonad.hs" = {
            # FIXME: reenable on-change recompilation after placing `xmonad` binary to PATH
            text = readSubstituted config inputs pkgs [ ./subst.nix ] [ ./assets/xmonad.hs ];
          };
        };
        xdg.configFile."xmobar/xmobarrc".text = readSubstituted config inputs pkgs [ ./subst.nix ] [ ./assets/xmobarrc ];
      };
    })
    (mkIf (cfg.enable && cfg.isDefault) {
      assertions = [
        {
          assertion = config.workstation.systemtraits.enable;
          message = "xmonad: must enable systemtraits maintenance.";
        }
        {
          assertion = !config.wm.awesome.isDefault && !config.wm.i3.isDefault && !config.wm.qtile.isDefault && !config.wm.stumpwm.isDefault;
          message = "xmonad: exactly one WM could be the default.";
        }
      ];

      shell.core.variables = [{ CURRENT_WM = "xmonad"; global = true; emacs = true; }];

      # TODO: ensure config dir exists (~/.xmonad or another if changeable)
      services.xserver = {
        windowManager = {
          xmonad = {
            enable = true;
            enableContribAndExtras = true;
            extraPackages = p: [ p.dbus p.monad-logger p.lens p.split ];
          };
        };
        displayManager = { defaultSession = "none+xmonad"; };
      };

      # FIXME: adopt new workspace keybindings implementation (presumably broken)
      wmCommon.keybindings.entries = {
        "M-C-q" = { cmd = "xmonad --recompile; xmonad --restart"; };
        "M-q" = { cmd = "xmonad --restart"; };
      };

      workstation.systemtraits.instructions = ''
        ${pkgs.redis}/bin/redis-cli set wm/keybindings ${
          lib.strings.escapeNixString (builtins.toJSON (cfg.internalKeys // config.wmCommon.keybindings.entries))
        }
      '';
    })
    (mkIf (cfg.enable && cfg.bookmarks.enable) {
      navigation.bookmarks.entries = {
        "google/xmonad" = {
          desc = "xmonad +";
          remote = {
            url = "https://www.google.ru/";
            searchSuffix = "?q=xmonad+";
          };
        };
      };
    })
    (mkIf (cfg.enable && config.completion.expansions.enable) {
      home-manager.users."${user}" = {
        xdg.configFile."espanso/match/xmonad.yml".source = yaml.generate "espanso-xmonad.yml"
          {
            matches = [
              {
                trigger = ":ygr";
                replace = "rm /home/${user}/.local/share/yeganesh/default";
              }
            ];
          } // optionalAttrs (config.shell.tmux.enable) {
          filter_title = "\".*${config.shell.tmux.defaultSession}.*${config.attributes.machine.name}.*\"";
        };
      };
    })
  ];
}
