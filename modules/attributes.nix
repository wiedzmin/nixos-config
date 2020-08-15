{ config, lib, pkgs, ... }:

with lib;

{
  options.attributes = {
    machine.name = mkOption {
      description = "Name of configuration under /etc/nixos/machines";
      type = types.str;
    };
    mainUser.name = mkOption {
      description = "Main user to be granted various service-related rights to";
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
    browser.fallback = mkOption {
      description = "Fallback browser command";
      type = types.str;
      default = "";
    };
    mimetypes.ebook = mkOption {
      description = "MIME types to handle as e-books";
      type = types.listOf types.str;
      default = [ "application/pdf" "image/vnd.djvu" ];
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
    nix.jobs = mkOption {
      description = "Simultaneous build jobs count";
      type = types.int;
      default = 4;
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
      };
    };
  };
}
