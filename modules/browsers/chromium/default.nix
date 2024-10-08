{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.browsers.chromium;
  user = config.attributes.mainUser.name;
  standardDesktopID = "chromium";
  windowedDesktopID = "org.custom.chromium.windowed";
  yaml = pkgs.formats.yaml { };
in
{
  options = {
    browsers.chromium = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable Chromium'.
        '';
      };
      traits = mkOption {
        type = types.submodule (import ../../workstation/systemtraits/xapp-traits.nix);
        description = "Chromium application traits";
      };
      isDefault = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Chromium should be the default browser.
        '';
      };
      isFallback = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Chromium should be fallback browser.
        '';
      };
      keyboardCentric = mkOption {
        type = types.bool;
        default = true;
        description = "Enable keyboard-centric controls (vimium, etc.)";
      };
      suspendInactive = mkOption {
        type = types.bool;
        default = true;
        description = "Suspend when inactive (using xsuspender)";
      };
      videoconferencing.enable = mkOption {
        type = types.bool;
        default = true;
        description = "Enable videoconferencing-related options";
      };
      desktopID = mkOption {
        type = types.enum [ standardDesktopID windowedDesktopID ];
        default = windowedDesktopID;
        description = "Desktop entry name";
      };
      # TODO: idea: advice link hint opening function to use "--new-tab" or such as argument after first link
      emacs.browseUrlSetup = mkOption {
        type = types.lines;
        default = ''
          (use-package browse-url
            :config
            (setq browse-url-browser-function 'browse-url-chromium)
            (setq browse-url-chromium-program "${cfg.traits.command.binary}")
            ${optionalString (cfg.traits.command.parameters != [ ]) "(setq browse-url-chromium-arguments '(${appCmdParametersQuotedSpaced cfg.traits}))"})
        '';
        visible = false;
        internal = true;
        description = "Specialized Chromium-aware `browse-url` package setup";
      };
    };
  };
  config = mkMerge [
    (mkIf cfg.enable {
      browsers.chromium.traits = rec {
        command = {
          binary = "${pkgs.chromium}/bin/chromium";
          parameters = [ "--new-window" ];
        };
        wmClass = [ "chromium-browser" "Chromium-browser" ];
        suspensionRule = {
          Chromium = {
            suspendDelay = 10;
            matchWmClassContains = lib.last wmClass;
            suspendSubtreePattern = binaryFromCmd (appCmdFull cfg.traits);
          };
        };
      };
      home-manager.users."${user}" = {
        # chrome-export
        programs.chromium = {
          enable = true;
          extensions = [
            "ignpacbgnbnkaiooknalneoeladjnfgb" # Url in title
            "naepdomgkenhinolocfifgehidddafch" # Browserpass
            "poahndpaaanbpbeafbkploiobpiiieko" # Display anchors
            # "cjpalhdlnbpafiamejdnhcphjbkeiagm" # uBlock Origin
            # "gcbommkclmclpchllfjekcdonpmejbdp" # HTTPS Everywhere
            # "ogfcmafjalglgifnmanfmnieipoejdcf" # uMatrix
          ] ++ optionals cfg.keyboardCentric [
            "dbepggeogbaibhgnhhndojpepiihcmeb" # Vimium
            # "gfbliohnnapiefjpjlpjnehglfpaknnc" # Surfingkeys
          ];
        };
        xdg.configFile = {
          "espanso/config/chromium.yml".source = yaml.generate "espanso-config-chromium.yml" {
            filter_class = appWindowClass cfg.traits;
            backend = "Clipboard";
          };
        };
        home.packages = with pkgs; [
          (makeDesktopItem {
            name = windowedDesktopID;
            type = "Application";
            exec = "${appCmdFull cfg.traits} %U";
            comment = "Chromium that opens links preferably in new windows";
            desktopName = "Chromium";
            categories = [ "Network" "WebBrowser" ];
          })
        ];
      };
      workstation.input.keyboard.xremap.config = {
        keymap = [
          {
            name = "${appName cfg.traits}";
            application = { only = "${appWindowClass cfg.traits}"; };
            remap = {
              "C-g" = "F5";
              "M-Comma" = "Shift-h";
              "M-dot" = "Shift-l";
            };
          }
        ];
      };
      workstation.input.keyboard.xkeysnail.rc = ''
        define_keymap(re.compile("${appWindowClass cfg.traits}"), {
            K("C-g"): K("f5"),
            K("M-comma"): K("Shift-h"),
            K("M-dot"): K("Shift-l"),
        }, "${appName cfg.traits}")
      '';
      # NOTE: use settings below to overcome Youtube slowdown
      # chrome://flags/#enable-quic - Enabled
      # chrome://flags/#enable-tls13-kyber - Disabled

      workstation.performance.appsSuspension.rules = optionalAttrs cfg.suspendInactive cfg.traits.suspensionRule;
    })
    (mkIf (cfg.enable && cfg.isDefault) {
      assertions = [
        {
          assertion = !config.browsers.firefox.isDefault && !config.browsers.nyxt.isDefault
            && !config.browsers.qutebrowser.isDefault;
          message = "browsers: chromium: there should be exactly one default.";
        }
      ];
      home-manager.users."${user}" = {
        xdg.mimeApps.defaultApplications = mapMimesToApp config.attributes.mimetypes.browser "${cfg.desktopID}.desktop";
      };
      services.xserver.displayManager.sessionCommands = ''
        ${pkgs.xdg-utils}/bin/xdg-settings set default-web-browser ${cfg.desktopID}.desktop
      '';
      shell.core.variables = [{ TB_DEFAULT_BROWSER = appCmdFull cfg.traits; global = true; }];

      attributes.browser.default.traits = cfg.traits;

      browsers.ext.emacs.browseUrlSetup = cfg.emacs.browseUrlSetup;
    })
    (mkIf (cfg.enable && cfg.isFallback) {
      assertions = [
        {
          assertion = !config.browsers.firefox.isFallback && !config.browsers.nyxt.isFallback
            && !config.browsers.qutebrowser.isFallback;
          message = "browsers: chromium: there should be exactly one fallback.";
        }
      ];
      attributes.browser.fallback.traits = cfg.traits;
      shell.core.variables = [{ TB_FALLBACK_BROWSER = appCmdFull cfg.traits; global = true; }];
    })
  ];
}
