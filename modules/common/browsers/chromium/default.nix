{ config, inputs, lib, pkgs, ... }:
with import ../../../util.nix { inherit config inputs lib pkgs; };

with lib;

let
  cfg = config.custom.browsers.chromium;
  user = config.attributes.mainUser.name;
  prefix = config.wmCommon.prefix;
in {
  options = {
    custom.browsers.chromium = {
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
      command = mkOption {
        type = types.str;
        default = "${pkgs.chromium}/bin/chromium --new-window";
        description = "Default command line to invoke";
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
      staging.enableSettings = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable staging settings.";
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
            "gfbliohnnapiefjpjlpjnehglfpaknnc" # Surfingkeys
            "ignpacbgnbnkaiooknalneoeladjnfgb" # Url in title
            "poahndpaaanbpbeafbkploiobpiiieko" # Display anchors
            # "cjpalhdlnbpafiamejdnhcphjbkeiagm" # uBlock Origin
            # "dbepggeogbaibhgnhhndojpepiihcmeb" # Vimium
            # "gcbommkclmclpchllfjekcdonpmejbdp" # HTTPS Everywhere
            # "naepdomgkenhinolocfifgehidddafch" # Browserpass
            # "ogfcmafjalglgifnmanfmnieipoejdcf" # uMatrix
          ];
        };
      };
      custom.pim.timeTracking.rules = ''
        -- TODO: parameterize web resources
        current window $program == ["Google-chrome", "Google-chrome-stable"] ==> tag activity:web,
        current window ($program == ["Google-chrome", "Google-chrome-stable"] && $title =~ /Facebook/) ==> tag site:facebook,
        current window ($program == ["Google-chrome", "Google-chrome-stable"] && $title =~ /Gmail/) ==> tag web:Gmail,
        current window ($program == ["Google-chrome", "Google-chrome-stable"] && $title =~ /Google/) ==> tag web:Google,
        current window ($program == ["Google-chrome", "Google-chrome-stable"] && $title =~ /wikipedia/) ==> tag site:wikipedia,
        current window ($program == ["Google-chrome", "Google-chrome-stable"] && $title =~ /habr/) ==> tag site:habr,
        current window ($program == ["Google-chrome", "Google-chrome-stable"] && $title =~ /pypi/) ==> tag site:pypi,
        current window ($program == ["Google-chrome", "Google-chrome-stable"] && $title =~ /stackoverflow/) ==> tag site:stackoverflow,
      '';
    })
    (mkIf (cfg.enable && cfg.isDefault) {
      assertions = [
        {
          assertion = !cfg.isFallback;
          message = "browsers: chromium: cannot be the default and fallback at the same time.";
        }
        {
          assertion = !config.custom.browsers.firefox.isDefault && !config.custom.browsers.next.isDefault
            && !config.custom.browsers.qutebrowser.isDefault;
          message = "browsers: chromium: there should be exactly one default.";
        }
      ];

      environment.sessionVariables = { BROWSER = cfg.command; };
      home-manager.users.${user} = {
        xdg.mimeApps.defaultApplications = mapMimesToApp config.attributes.mimetypes.browser "chromium.desktop";
      };
      attributes.browser.default = cfg.command;
    })
    (mkIf (cfg.enable && cfg.isFallback) {
      assertions = [
        {
          assertion = !cfg.isDefault;
          message = "browsers: chromium: cannot be the default and fallback at the same time.";
        }
        {
          assertion = !config.custom.browsers.firefox.isFallback && !config.custom.browsers.next.isFallback
            && !config.custom.browsers.qutebrowser.isFallback;
          message = "browsers: chromium: there should be exactly one fallback.";
        }
      ];
      attributes.browser.fallback = cfg.command;
    })
  ];
}
