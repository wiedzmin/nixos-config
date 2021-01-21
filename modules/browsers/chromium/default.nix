{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };

with lib;

let
  cfg = config.browsers.chromium;
  user = config.attributes.mainUser.name;
  suspensionRule = {
    Chromium = {
      suspendDelay = 10;
      matchWmClassContains = "Chromium-browser";
      suspendSubtreePattern = "chromium";
    };
  };
in {
  options = {
    browsers.chromium = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = ''
          Whether to enable Chromium'.
        '';
      };
      downloadPath = mkOption {
        type = types.str;
        default = homePrefix "Downloads";
        description = ''
          Downloads path'.
        '';
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
        description = "Suspend when inacive (using xsuspender)";
      };
      command = mkOption {
        type = types.str;
        default = "${pkgs.chromium}/bin/chromium --new-window";
        description = "Default command line to invoke";
      };
      windowClass = mkOption {
        type = types.listOf types.str;
        default = [ "Chromium-browser" ];
        visible = false;
        internal = true;
        description = "Chromium default window class.";
      };
      extraOpts = mkOption { # FIXME: resurrect usage
        type = types.attrs;
        description = ''
          Extra chromium policy options, see
          <link xlink:href="https://www.chromium.org/administrators/policy-list-3">https://www.chromium.org/administrators/policy-list-3</link>
          for a list of available options
        '';
        default = { };
      };
    };
  };
  config = mkMerge [
    (mkIf cfg.enable {
      home-manager.users.${user} = {
        # chrome-export
        programs.browserpass.enable = true;
        programs.chromium = {
          enable = true;
          extensions = [
            "ignpacbgnbnkaiooknalneoeladjnfgb" # Url in title
            "naepdomgkenhinolocfifgehidddafch" # Browserpass
            "poahndpaaanbpbeafbkploiobpiiieko" # Display anchors
            # "cjpalhdlnbpafiamejdnhcphjbkeiagm" # uBlock Origin
            # "gcbommkclmclpchllfjekcdonpmejbdp" # HTTPS Everywhere
            # "ogfcmafjalglgifnmanfmnieipoejdcf" # uMatrix
          ] ++ optionals (cfg.keyboardCentric) [
            "dbepggeogbaibhgnhhndojpepiihcmeb" # Vimium
            # "gfbliohnnapiefjpjlpjnehglfpaknnc" # Surfingkeys
          ];
        };
      };
      workstation.input.xkeysnail.rc = ''
        define_keymap(re.compile("Chromium-browser"), {
            K("C-g"): K("f5"),
        }, "chromium")
      '';

    })
    (mkIf (cfg.enable && cfg.isDefault) {
      assertions = [
        {
          assertion = !cfg.isFallback;
          message = "browsers: chromium: cannot be the default and fallback at the same time.";
        }
        {
          assertion = !config.browsers.firefox.isDefault && !config.browsers.nyxt.isDefault
            && !config.browsers.qutebrowser.isDefault;
          message = "browsers: chromium: there should be exactly one default.";
        }
        {
          assertion = config.browsers.core.enable;
          message = "browsers/chromium: must enable browsers/core.";
        }
      ];

      environment.sessionVariables = { BROWSER = cfg.command; };
      home-manager.users.${user} = {
        xdg.mimeApps.defaultApplications = mapMimesToApp config.attributes.mimetypes.browser "chromium.desktop";
      };
      attributes.browser.default.cmd = cfg.command;
      attributes.browser.default.windowClass = cfg.windowClass;

      workstation.performance.appsSuspension.rules = optionalAttrs (cfg.suspendInactive) suspensionRule;
    })
    (mkIf (cfg.enable && cfg.isFallback) {
      assertions = [
        {
          assertion = !cfg.isDefault;
          message = "browsers: chromium: cannot be the default and fallback at the same time.";
        }
        {
          assertion = !config.browsers.firefox.isFallback && !config.browsers.nyxt.isFallback
            && !config.browsers.qutebrowser.isFallback;
          message = "browsers: chromium: there should be exactly one fallback.";
        }
        {
          assertion = config.browsers.core.enable;
          message = "browsers/chromium: must enable browsers/core.";
        }
      ];
      attributes.browser.fallback.cmd = cfg.command;
      attributes.browser.fallback.windowClass = cfg.windowClass;

      workstation.performance.appsSuspension.rules = optionalAttrs (cfg.suspendInactive) suspensionRule;
    })
  ];
}
