{ config, inputs, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.wm.awesome;
  user = config.attributes.mainUser.name;
  getLuaPath = lib: dir: "${lib}/${dir}/lua/${pkgs.luaPackages.lua.luaversion}";
  makeSearchPath = concatMapStrings (path:
    " --search " + (getLuaPath path "share") +
    " --search " + (getLuaPath path "lib")
  );
  inherit (config.wmCommon) prefix;
in
{
  options = {
    wm.awesome = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable AwesomeWM";
      };
      isDefault = mkOption {
        type = types.bool;
        default = false;
        description = "Set `AwesomeWM` as default WM";
      };
      luaModules = mkOption {
        default = [ ];
        type = types.listOf types.package;
        description = "List of lua packages available for being used in the Awesome configuration.";
        example = literalExpression "[ luaPackages.oocairo ]";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      fonts.fonts = with pkgs; [ font-awesome ];

      nixpkgs.config.packageOverrides = _: rec {
        debug-awesome = mkWMDebugScript
          pkgs "debug-awesome"
          pkgs.awesome
          config.attributes.hardware.monitors.internalHead
          ''awesome -c "$XDG_CONFIG_HOME/awesome/rc.lua" ${makeSearchPath cfg.luaModules}'';
      };

      # TODO: implement respective Lua code generation
      wmCommon.keybindings.common = [
        {
          key = [ prefix "Escape" ];
          cmd = ''
            awful.tag.history.restore()
          '';
          mode = "root";
          raw = true;
        }
        {
          key = [ prefix "Left" ];
          cmd = ''
            awful.client.focus.global_bydirection('left')
          '';
          mode = "root";
          raw = true;
        }
        {
          key = [ prefix "Right" ];
          cmd = ''
            awful.client.focus.global_bydirection('right')
          '';
          mode = "root";
          raw = true;
        }
        {
          key = [ prefix "Up" ];
          cmd = ''
            awful.client.focus.global_bydirection('up')
          '';
          mode = "root";
          raw = true;
        }
        {
          key = [ prefix "Down" ];
          cmd = ''
            awful.client.focus.global_bydirection('down')
          '';
          mode = "root";
          raw = true;
        }
        {
          key = [ prefix "Control" "Left" ];
          cmd = ''
            awful.client.swap.global_bydirection('left')
          '';
          mode = "root";
          raw = true;
        }
        {
          key = [ prefix "Control" "Right" ];
          cmd = ''
            awful.client.swap.global_bydirection('right')
          '';
          mode = "root";
          raw = true;
        }
        {
          key = [ prefix "Control" "Up" ];
          cmd = ''
            awful.client.swap.global_bydirection('up')
          '';
          mode = "root";
          raw = true;
        }
        {
          key = [ prefix "Control" "Down" ];
          cmd = ''
            awful.client.swap.global_bydirection('down')
          '';
          mode = "root";
          raw = true;
        }
        {
          key = [ prefix "," ];
          cmd = ''
            awful.screen.focus_bydirection("left")
          '';
          mode = "root";
          raw = true;
        }
        {
          key = [ prefix "," ];
          cmd = ''
            awful.screen.focus_bydirection("left")
          '';
          mode = "root";
          raw = true;
        }
        {
          key = [ prefix "." ];
          cmd = ''
            awful.screen.focus_bydirection("right")
          '';
          mode = "root";
          raw = true;
        }
        {
          key = [ prefix "u" ];
          cmd = ''
            awful.client.urgent.jumpto()
          '';
          mode = "root";
          raw = true;
        }
        {
          key = [ prefix "h" ];
          cmd = ''
            awful.tag.incmwfact(-0.05)
          '';
          mode = "root";
          raw = true;
        }
        {
          key = [ prefix "l" ];
          cmd = ''
            awful.tag.incmwfact(0.05)
          '';
          mode = "root";
          raw = true;
        }
        {
          key = [ prefix "Shift" "h" ];
          cmd = ''
            awful.tag.incnmaster(1)
          '';
          mode = "root";
          raw = true;
        }
        {
          key = [ prefix "Shift" "l" ];
          cmd = ''
            awful.tag.incnmaster(-1)
          '';
          mode = "root";
          raw = true;
        }
        {
          key = [ prefix "Control" "h" ];
          cmd = ''
            awful.tag.incncol(1)
          '';
          mode = "root";
          raw = true;
        }
        {
          key = [ prefix "Control" "l" ];
          cmd = ''
            awful.tag.incncol(-1)
          '';
          mode = "root";
          raw = true;
        }
        # -- ['M-<Space>'] = function () awful.layout.inc(layouts, 1) end, -- TODO: fix signature
        # -- ['M-S-<Space>'] = function () awful.layout.inc(layouts, -1) end, -- TODO: fix signature
        {
          key = [ prefix "x" ];
          cmd = ''
            awful.prompt.run {
                prompt       = "Run Lua code: ",
                textbox      = awful.screen.focused().mypromptbox.widget,
                exe_callback = awful.util.eval,
                history_path = awful.util.get_cache_dir() .. "/history_eval"
            }
          '';
          mode = "root";
          raw = true;
        }
        {
          key = [ prefix "p" ];
          cmd = ''
            menubar.show()
          '';
          mode = "root";
          raw = true;
        }
        {
          key = [ prefix "Escape" ];
          cmd = ''
            menus.show_apps_menu()
          '';
          mode = "root";
          raw = true;
        }
        {
          key = [ "Control" "\\" ];
          cmd = ''
            utils.toggle_keyboard_layout()
          '';
          mode = "root";
          raw = true;
        }
        {
          key = [ prefix "e" ];
          cmd = ''
            hints.focus()
          '';
          mode = "root";
          raw = true;
        }
        # { # TODO: package and add `cheeky` beforehand
        #   key = [ prefix "Shift" "/" ];
        #   cmd = ''
        #     cheeky.util.switcher()
        #   '';
        #   mode = "root";
        #   raw = true;
        # }
        {
          key = [ prefix "Control" "r" ];
          cmd = ''
            awesome.restart()
          '';
          mode = "root";
          raw = true;
        }
        {
          key = [ prefix "Shift" "q" ];
          cmd = ''
            awesome.quit()
          '';
          mode = "root";
          raw = true;
        }
      ];

      home-manager.users."${user}" = {
        home.packages = with pkgs; [ debug-awesome ];
        xdg.configFile = {
          "awesome/rc.lua".text = readSubstituted config inputs pkgs [ ./subst.nix ] [ ./rc.lua ];
          "awesome/lib/controls.lua".text = readSubstituted config inputs pkgs [ ./subst.nix ] [ ./lib/controls.lua ];
          "awesome/lib/menus.lua".text = readSubstituted config inputs pkgs [ ./subst.nix ] [ ./lib/menus.lua ];
          "awesome/lib/themes.lua".text = readSubstituted config inputs pkgs [ ./subst.nix ] [ ./lib/themes.lua ];
          "awesome/lib/utils.lua".text = readSubstituted config inputs pkgs [ ./subst.nix ] [ ./lib/utils.lua ];
          "awesome/lib/widgets.lua".text = readSubstituted config inputs pkgs [ ./subst.nix ] [ ./lib/widgets.lua ];
          "awesome/lib/windows.lua".text = readSubstituted config inputs pkgs [ ./subst.nix ] [ ./lib/windows.lua ];
          "awesome/themes" = { source = ./themes; recursive = true; };
          "awesome/lib/debug.lua".text = readSubstituted config inputs pkgs [ ./subst.nix ] [ ./lib/debug.lua ];
        };
      };
    })
    (mkIf (cfg.enable && cfg.isDefault) {
      assertions = [{
        assertion = !config.wm.i3.isDefault && !config.wm.qtile.isDefault && !config.wm.stumpwm.isDefault && !config.wm.xmonad.isDefault;
        message = "awesome: exactly one WM could be the default.";
      }];

      shell.core.variables = [{ CURRENT_WM = "awesome"; global = true; emacs = true; }];

      services.xserver = {
        windowManager = {
          awesome = {
            enable = true;
            luaModules = cfg.luaModules;
          };
        };
        displayManager = { defaultSession = "none+awesome"; };
      };
    })
  ];
}
