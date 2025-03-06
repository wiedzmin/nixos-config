{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;
with lib;

let
  cfg = config.browsers.chromium;
  user = config.attributes.mainUser.name;
  standardDesktopID = "chromium";
  windowedDesktopID = "org.custom.chromium.windowed";
  suspensionRule = {
    Chromium = {
      suspendDelay = 10;
      matchWmClassContains = lib.last cfg.windowClass;
      suspendSubtreePattern = binaryFromCmd cfg.command;
    };
  };
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
      command = mkOption {
        type = types.str;
        default = "${pkgs.chromium}/bin/chromium --new-window";
        description = "Default command line to invoke";
      };
      windowClass = mkOption {
        type = types.listOf types.str;
        default = [ "chromium-browser" "Chromium-browser" ];
        visible = false;
        internal = true;
        description = "Chromium default window class.";
      };
      desktopID = mkOption {
        type = types.enum [ standardDesktopID windowedDesktopID ];
        default = windowedDesktopID;
        description = "Desktop entry name";
      };
      # TODO: idea: advice link hint opening function to use "--new-tab" or such as argument after first link
      # FIXME: consider refactoring `cfg.command` representation above
      emacs.browseUrlSetup = mkOption {
        type = types.lines;
        default = ''
          (use-package browse-url
            :config
            (setq browse-url-browser-function 'browse-url-chromium)
            (setq browse-url-chromium-program "${builtins.head (splitString " " cfg.command)}")
            (setq browse-url-chromium-arguments '("${concatStringsSep " " (forEach (builtins.tail (splitString " " cfg.command))
              (s: "\"" + s + "\""))}")))
        '';
        visible = false;
        internal = true;
        description = "Specialized Chromium-aware `browse-url` package setup";
      };
      extraOpts = mkOption {
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
            "lhaoghhllmiaaagaffababmkdllgfcmc" # Atomic Chrome
          ] ++ optionals cfg.keyboardCentric [
            "dbepggeogbaibhgnhhndojpepiihcmeb" # Vimium
            # "gfbliohnnapiefjpjlpjnehglfpaknnc" # Surfingkeys
          ];
        };
        xdg.configFile = {
          "espanso/config/chromium.yml".source = yaml.generate "espanso-config-chromium.yml" {
            filter_class = lib.last cfg.windowClass;
            backend = "Clipboard";
          };
        };
        home.packages = with pkgs; [
          (makeDesktopItem {
            name = windowedDesktopID;
            type = "Application";
            exec = "${cfg.command} %U";
            comment = "Chromium that opens links preferably in new windows";
            desktopName = "Chromium";
            categories = [ "Network" "WebBrowser" ];
          })
        ];
      };
      workstation.input.keyboard.xkeysnail.rc = ''
        define_keymap(re.compile("${lib.last cfg.windowClass}"), {
            K("C-g"): K("f5"),
            K("M-comma"): K("Shift-h"),
            K("M-dot"): K("Shift-l"),
        }, "chromium")
      '';
      browsers.chromium.extraOpts = {
        AutofillAddressEnabled = false;
        AutofillCreditCardEnabled = false;
        AutoplayAllowed = false;
        BrowserSignin = 0; # Disable browser sign-in
        BuiltInDnsClientEnabled = false;
        DefaultBrowserSettingEnabled = false;
        DefaultGeolocationSetting = 2; # Do not allow any site to track the users' physical location
        DefaultNotificationsSetting = 2; # Do not allow any site to show desktop notifications
        DefaultPluginsSetting = 2; # Block the Flash plugin
        DefaultSearchProviderEnabled = true;
        DefaultSearchProviderSearchURL = "https://duckduckgo.com/"
          + "?kae=d&k1=-1&kc=1&kav=1&kd=-1&kh=1&q={searchTerms}";
        EnableMediaRouter = false;
        MetricsReportingEnabled = false;
        PasswordManagerEnabled = false;
        PromotionalTabsEnabled = false;
        SSLErrorOverrideAllowed = false;
        SafeBrowsingEnabled = false;
        SearchSuggestEnabled = false;
        SigninAllowed = false;
        SpellCheckServiceEnabled = false;
        SpellcheckEnabled = false;
        SyncDisabled = true;
        TranslateEnabled = false;
        ExternalProtocolDialogShowAlwaysOpenCheckbox = true;
      };
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
      shell.core.variables = [{ TB_DEFAULT_BROWSER = cfg.command; global = true; }];

      attributes.browser.default.cmd = cfg.command;
      attributes.browser.default.windowClass = cfg.windowClass;

      browsers.ext.emacs.browseUrlSetup = cfg.emacs.browseUrlSetup;

      workstation.performance.appsSuspension.rules = optionalAttrs cfg.suspendInactive suspensionRule;
    })
    (mkIf (cfg.enable && cfg.isFallback) {
      assertions = [
        {
          assertion = !config.browsers.firefox.isFallback && !config.browsers.nyxt.isFallback
            && !config.browsers.qutebrowser.isFallback;
          message = "browsers: chromium: there should be exactly one fallback.";
        }
      ];
      attributes.browser.fallback.cmd = cfg.command;
      attributes.browser.fallback.windowClass = cfg.windowClass;
      shell.core.variables = [{ TB_FALLBACK_BROWSER = cfg.command; global = true; }];

      workstation.performance.appsSuspension.rules = optionalAttrs cfg.suspendInactive suspensionRule;
    })
    (mkIf (cfg.enable && cfg.videoconferencing.enable) {
      browsers.chromium.extraOpts = {
        AudioCaptureAllowed = true;
        VideoCaptureAllowed = true;
      };
    })
  ];
}
