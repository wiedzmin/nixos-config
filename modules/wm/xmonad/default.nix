{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.wm.xmonad;
  user = config.attributes.mainUser.name;
  yaml = pkgs.formats.yaml { };
  inherit (config.wmCommon) prefix;
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

      wmCommon = {
        modeBindings = {
          # FIXME: review mode bindings
          "workspace" = [ prefix "Shift" "w" ];
          "window" = [ prefix "Shift" "n" ];
        };
        keybindings.entries = (forEach [
          {
            key = [ "Control" "backslash" ];
            cmd = ''sendMessage (XkbToggle Nothing)'';
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "Home" ];
            cmd = ''toggleWS'';
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "Return" ];
            cmd = ''promote'';
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "Space" ];
            cmd = ''sendMessage NextLayout'';
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "Tab" ];
            cmd = ''windows W.focusDown'';
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "Shift" "Space" ];
            cmd = ''setLayout $ XMonad.layoutHook conf'';
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "Shift" "Tab" ];
            cmd = ''windows W.focusUp'';
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "F12" ];
            cmd = ''kill1'';
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "Shift" "j" ];
            cmd = ''windows W.swapDown'';
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "Shift" "k" ];
            cmd = ''windows W.swapUp'';
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "Shift" "q" ];
            cmd = ''io (exitWith ExitSuccess)'';
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "h" ];
            cmd = ''sendMessage Shrink'';
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "l" ];
            cmd = ''sendMessage Expand'';
            mode = "root";
            raw = true;
          }
          {
            key = [ prefix "t" ];
            cmd = ''withFocused $ windows . W.sink'';
            mode = "root";
            raw = true;
          }
          {
            key = [ "a" ];
            cmd = ''windows copyToAll''; # @@ Make focused window always visible
            mode = "workspace";
            raw = true;
          }
          {
            key = [ "k" ];
            cmd = ''killAllOtherCopies''; # @@ Toggle window state back
            mode = "workspace";
            raw = true;
          }
          {
            key = [ "1" ];
            cmd = ''namedScratchpadAction scratchpads "htop"'';
            mode = "scratchpad";
            raw = true;
          }
          {
            key = [ "2" ];
            cmd = ''namedScratchpadAction scratchpads "iotop"'';
            mode = "scratchpad";
            raw = true;
          }
          {
            key = [ "3" ];
            cmd = ''namedScratchpadAction scratchpads "gotop"'';
            mode = "scratchpad";
            raw = true;
          }
          {
            key = [ "4" ];
            cmd = ''namedScratchpadAction scratchpads "bc"'';
            mode = "scratchpad";
            raw = true;
          }
          {
            key = [ "5" ];
            cmd = ''namedScratchpadAction scratchpads "redis"'';
            mode = "scratchpad";
            raw = true;
          }
          # TODO: recall and add "multiple app windows"-aware raising
          {
            key = [ "Backspace" ];
            cmd = ''nextMatch History (return True)'';
            mode = "window";
            raw = true;
          }
          {
            key = [ "M-S-n" ];
            cmd = ''moveTo Next EmptyWS''; # find a free workspace
            mode = "workspace";
            raw = true;
          }
          {
            key = [ "M-S-<Up>" ];
            cmd = ''shiftToPrev'';
            mode = "workspace";
            raw = true;
          }
          {
            key = [ "M-S-<Down>" ];
            cmd = ''shiftToNext'';
            mode = "workspace";
            raw = true;
          }
          {
            key = [ "M-S-<Right>" ];
            cmd = ''shiftToNext >> nextWS'';
            mode = "workspace";
            raw = true;
          }
          {
            key = [ "M-S-<Left>" ];
            cmd = ''shiftToPrev >> prevWS'';
            mode = "workspace";
            raw = true;
          }
          {
            key = [ "<Up>" ];
            cmd = ''prevWS'';
            mode = "workspace";
            raw = true;
          }
          {
            key = [ "<Down>" ];
            cmd = ''nextWS'';
            mode = "workspace";
            raw = true;
          }
          {
            key = [ "M-s" ];
            cmd = ''sinkAll'';
            mode = "window";
            raw = true;
          }
          {
            key = [ "r" ];
            cmd = ''refresh'';
            mode = "window";
            raw = true;
          }
          {
            key = [ "M-w" ];
            cmd = ''placeWorkplaces'';
            mode = "workspace";
            raw = true;
          }
          {
            key = [ "M-S-." ];
            cmd = ''placeFocused placePolicy'';
            mode = "root";
            raw = true;
          }
          {
            key = [ "M-<Right>" ];
            cmd = ''windowGo R True'';
            mode = "root";
            raw = true;
          }
          {
            key = [ "M-<Left>" ];
            cmd = ''windowGo L True'';
            mode = "root";
            raw = true;
          }
          {
            key = [ "M-<Up>" ];
            cmd = ''windowGo U True'';
            mode = "root";
            raw = true;
          }
          {
            key = [ "M-<Down>" ];
            cmd = ''windowGo D True'';
            mode = "root";
            raw = true;
          }
        ]
          (e: e // { wm = "xmonad"; }));
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
