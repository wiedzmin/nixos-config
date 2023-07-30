{ lib, ... }:

with lib;

{
  options.attributes = {
    machine.name = mkOption {
      description = "Name of configuration under <config root>/machines";
      type = types.str;
    };
    mainUser.name = mkOption {
      description = "Main user to be granted various service-related rights to";
      type = types.str;
    };
    mainUser.ID = mkOption {
      description = "Main user ID (see above)";
      type = types.str;
    };
    mainUser.fullName = mkOption {
      description = "Main user's full name";
      type = types.str;
    };
    mainUser.email = mkOption {
      description = "Main user's email";
      type = types.str;
    };
    mainUser.gpgKeyID = mkOption {
      description = "Main user's GPG key ID";
      type = types.str;
    };
    secret = mkOption {
      description = "Secret data placeholder";
      default = "0xDEADF00D";
      type = types.str;
    };
    localGroup = mkOption {
      description = "Default local group name";
      type = types.str;
      default = "users";
    };
    mimetypes.browser = mkOption {
      description = "MIME types to handle with (default) browser";
      type = types.listOf types.str;
      default = [
        "text/html"
        "x-scheme-handler/http"
        "x-scheme-handler/https"
        "application/x-extension-htm"
        "application/x-extension-html"
        "application/x-extension-shtml"
        "application/xhtml+xml"
        "application/x-extension-xht"
        "x-scheme-handler/about"
        "x-scheme-handler/unknown"
      ];
    };
    browser.default.cmd = mkOption {
      description = "Default browser command";
      type = types.str;
      default = "";
    };
    browser.default.windowClass = mkOption {
      description = "Default browser window class";
      type = types.listOf types.str;
      default = [ ];
    };
    browser.fallback.cmd = mkOption {
      description = "Fallback browser command";
      type = types.str;
      default = "";
    };
    browser.fallback.windowClass = mkOption {
      description = "Fallback browser window class";
      type = types.listOf types.str;
      default = [ ];
    };
    ebookreader.default.cmd = mkOption {
      description = "Default ebook reader command";
      type = types.str;
      default = "";
    };
    ebookreader.default.windowClass = mkOption {
      description = "Default ebook reader window class";
      type = types.listOf types.str;
      default = [ ];
    };
    ebookreader.fallback.cmd = mkOption {
      description = "Fallback ebook reader command";
      type = types.str;
      default = "";
    };
    ebookreader.fallback.windowClass = mkOption {
      description = "Fallback ebook reader window class";
      type = types.listOf types.str;
      default = [ ];
    };
    pager.cmd = mkOption {
      description = "Pager command";
      type = types.str;
      default = "";
    };
    gitPager.cmd = mkOption {
      description = "Pager command to use with Git";
      type = types.str;
      default = "";
    };
    mimetypes.ebook = mkOption {
      description = "MIME types to handle as e-books";
      type = types.listOf types.str;
      default = [ "application/pdf" "image/vnd.djvu" "application/epub+zip" ];
    };
    mimetypes.images = mkOption {
      description = "MIME types to handle as graphic images";
      type = types.listOf types.str;
      default = [ "image/png" "image/jpg" "image/jpeg" ];
    };
    mimetypes.video = mkOption {
      description = "MIME types to handle as video";
      type = types.listOf types.str;
      default = [ "video/3gpp" "video/mp4" "video/quicktime" "video/x-flv" "video/x-matroska" "video/x-msvideo" ];
    };
    mimetypes.office.docs = mkOption {
      description = "MIME types to handle as documents";
      type = types.listOf types.str;
      default = [
        "application/msword"
        "application/vnd.ms-word.document.macroEnabled.12"
        "application/vnd.ms-word.template.macroEnabled.12"
        "application/vnd.openxmlformats-officedocument.wordprocessingml.document"
        "application/vnd.openxmlformats-officedocument.wordprocessingml.template"
      ];
    };
    mimetypes.office.spreadsheets = mkOption {
      description = "MIME types to handle as spreadsheets";
      type = types.listOf types.str;
      default = [
        "application/vnd.ms-excel"
        "application/vnd.ms-excel.addin.macroEnabled.12"
        "application/vnd.ms-excel.sheet.binary.macroEnabled.12"
        "application/vnd.ms-excel.sheet.macroEnabled.12"
        "application/vnd.ms-excel.template.macroEnabled.12"
        "application/vnd.openxmlformats-officedocument.spreadsheetml.sheet"
        "application/vnd.openxmlformats-officedocument.spreadsheetml.template"
      ];
    };
    debug.enable = mkOption {
      description = "Whether to enable unfinished/in-development/broken custom scripts/packages";
      type = types.bool;
      default = false;
    };
    hardware.cores = mkOption {
      description = "CPU cores count";
      type = types.int;
      default = 4;
    };
    hardware.dmiSystemVersion = mkOption {
      description = "Hardware system definition, as shown by `dmidecode -s system-version`";
      type = types.str;
      default = "";
    };
    nix.jobs = mkOption {
      description = "Simultaneous build jobs count";
      type = types.int;
      default = 4;
    };
    debug.scripts = mkOption {
      description = "Whethet to expose custom scripts to shell for debugging";
      type = types.bool;
      default = false;
    };
    hardware.monitors = {
      internalHead = {
        name = mkOption {
          type = types.str;
          default = "";
          description = "Internal laptop head name";
        };
        edid = mkOption {
          type = types.str;
          default = "";
          description = "Internal laptop head EDID value";
        };
        resolution = mkOption {
          type = types.str;
          default = "";
          description = "Internal laptop head resolution";
        };
        resolutionXephyr = mkOption {
          type = types.str;
          default = "";
          description = "Internal laptop head resolution when running Xephyr with no external head attached";
        };
      };
      externalPrimaryHead = {
        name = mkOption {
          type = types.str;
          default = "";
          description = "External primary head name";
        };
      };
      externalSecondaryHead = {
        name = mkOption {
          type = types.str;
          default = "";
          description = "External secondary head name";
        };
      };
      count = mkOption {
        type = types.int;
        default = 1;
        description = ''
          Overall available monitors count, including internal one.

          Dock stations should update this according to their video outputs.
        '';
      };
      layouts = mkOption {
        type = types.attrs;
        default = { };
        description = "Collection of all xrandr heads spatial layouts";
      };
    };
    dateFormats = mkOption {
      type = types.attrsOf types.str;
      default = {
        commonShell = "+%Y-%m-%d_%H:%M:%S";
        commonShellNoColons = "+%Y-%m-%d_%H-%M-%S";
        flameshot = "%Y-%m-%d_%H-%M-%S";
      };
      visible = false;
      readOnly = true;
      internal = true;
      description = "Date suffix formats for various needs";
    };
    regexes = mkOption {
      description = "Common regular expression";
      type = types.attrs;
      default = { url = { py = "(https?|ftp|file)://[-A-Za-z0-9+&@#/%?=~_|!:,.;]*[-A-Za-z0-9+&@#/%=~_|]"; }; };
    };
    vt.default.cmd = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = "Default virtual terminal command elements (including `command` flag, for executing something)";
    };
    vt.default.windowClass = mkOption {
      description = "Default virtual terminal window class";
      type = types.listOf types.str;
      default = [ ];
    };
    wms.enabled = mkOption {
      description = "Whether any window manager is enabled in current setup";
      type = types.bool;
      default = false;
    };
    hardware.inputDevices.keyboard = mkOption {
      type = types.listOf types.str;
      default = [ ];
      example = literalExpression ''
        [
          "/dev/input/event3"
        ]
      '';
      description = "Input devices to listen for keyboard events";
    };
    hardware.inputDevices.mouse = mkOption {
      type = types.listOf types.str;
      default = [ ];
      example = literalExpression ''
        [
          "/dev/input/event16"
        ]
      '';
      description = "Input devices to listen for mouse events";
    };
    hardware.memory.swap.enable = mkOption {
      type = types.bool;
      default = false;
      description = "Whether to enable memory swapping";
    };
    workspace.assets.subpath = mkOption {
      description = "Subpath to store workspace-related assets under";
      type = types.str;
      default = "workspace/assets";
    };
  };
}
