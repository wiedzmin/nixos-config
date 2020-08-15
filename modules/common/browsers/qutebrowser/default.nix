let
  deps = import ../../../../nix/sources.nix;
  nixpkgs-pinned-16_04_20 = import deps.nixpkgs-pinned-16_04_20 { config.allowUnfree = true; };
in { config, lib, pkgs, ... }:
with import ../../../util.nix { inherit config lib pkgs; };

with lib;

let
  cfg = config.custom.browsers.qutebrowser;
  prefix = config.wmCommon.prefix;
in {
  options = {
    custom.browsers.qutebrowser = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable qutebrowser.";
      };
      downloadPath = mkOption {
        type = types.str;
        default = homePrefix "Downloads";
        description = "Downloads path.";
      };
      isDefault = mkOption {
        type = types.bool;
        default = false;
        description = "Set qutebrowser as default browser.";
      };
      isFallback = mkOption {
        type = types.bool;
        default = false;
        description = "Set qutebrowser as fallback browser.";
      };
      command = mkOption {
        type = types.str;
        default = "${nixpkgs-pinned-16_04_20.qutebrowser}/bin/qutebrowser --target window";
        description = "Default command line to invoke";
      };
      staging.enableSettings = mkOption {
        type = types.bool;
        default = false;
        description = "Enable staging settings.";
      };
    };
  };
  config = mkMerge [
    (mkIf cfg.enable {
      nixpkgs.config.packageOverrides = _: rec {
        yank-image = mkShellScriptWithDeps "yank-image" (with pkgs; [ wget xclip ]) (builtins.readFile
          (pkgs.substituteAll
            ((import ../../subst.nix { inherit config pkgs lib; }) // { src = ./scripts/yank-image.sh; })));
        qb-fix-session = mkPythonScriptWithDeps "qb-fix-session" (with pkgs; [ python3Packages.pyyaml ])
          (builtins.readFile (pkgs.substituteAll
            ((import ../../subst.nix { inherit config pkgs lib; }) // { src = ./scripts/qb-fix-session.py; })));
      };
      custom.xinput.xkeysnail.rc = ''
        define_keymap(re.compile("qutebrowser"), {
            K("C-g"): K("f5"),
            K("C-n"): K("C-g"),
            K("M-comma"): K("Shift-h"),
            K("M-dot"): K("Shift-l"),
            K("C-x"): {
                K("b"): K("b"),
                K("k"): [K("Esc"), K("d")],
                K("u"): K("u"),
                K("C-s"): [K("Esc"), K("Shift-semicolon"), K("w"), K("enter")],
                K("C-c"): [K("Esc"), K("Shift-semicolon"), K("w"), K("q"), K("enter")],
            },
        }, "qutebrowser")
      '';
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          nixpkgs-pinned-16_04_20.qutebrowser
          yank-image
          qb-fix-session

          (makeDesktopItem {
            name = "org.custom.qutebrowser.windowed";
            type = "Application";
            exec = "${cfg.command} %U";
            comment = "Qutebrowser that opens links preferably in new windows";
            desktopName = "QuteBrowser";
            categories = stdenv.lib.concatStringsSep ";" [ "Network" "WebBrowser" ];
          })
        ];
        xdg.configFile = { # FIXME: migrate to nix-attributes settings
          "qutebrowser/config.py".text = builtins.readFile (pkgs.substituteAll
            ((import ../../subst.nix { inherit config pkgs lib; }) // { src = ./config/qutebrowser.py; }));
          "qutebrowser/keybindings.py".text = builtins.readFile (pkgs.substituteAll
            ((import ../../subst.nix { inherit config pkgs lib; }) // { src = ./config/keybindings.py; }));
        };
      };
    })
    (mkIf (cfg.enable && cfg.isDefault) {
      assertions = [
        {
          assertion = !cfg.isFallback;
          message = "browsers: qutebrowser: cannot be the default and fallback at the same time.";
        }
        {
          assertion = !config.custom.browsers.chromium.isDefault && !config.custom.browsers.next.isDefault
            && !config.custom.browsers.firefox.isDefault;
          message = "browsers: qutebrowser: there should be exactly one default.";
        }
      ];

      home-manager.users."${config.attributes.mainUser.name}" = {
        xdg.mimeApps.defaultApplications =
          mapMimesToApp config.attributes.mimetypes.browser "org.custom.qutebrowser.windowed.desktop";
      };
    })
    (mkIf (cfg.enable && cfg.isFallback) {
      assertions = [
        {
          assertion = !cfg.isDefault;
          message = "browsers: qutebrowser: cannot be the default and fallback at the same time.";
        }
        {
          assertion = !config.custom.browsers.chromium.isFallback && !config.custom.browsers.next.isFallback
            && !config.custom.browsers.firefox.isFallback;
          message = "browsers: qutebrowser: there should be exactly one fallback.";
        }
      ];
      attributes.browser.fallback = cfg.command;
    })
  ];
}
