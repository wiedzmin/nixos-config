{ lib, ... }:
let
  inherit (lib) mkOption types;
in
{
  options = {
    command.binary = mkOption {
      type = types.str;
      default = "";
      description = "X application binary";
    };
    command.type = mkOption {
      type = types.enum [ "browser" "terminal" ];
      description = "X application type";
    };
    command.parameters = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = "X application command parameters list";
    };
    terminal.wrapper.template = mkOption {
      type = types.str;
      default = "";
      description = "Command template for running TUI applications";
    };
    terminal.bare.template = mkOption {
      type = types.str;
      default = "";
      description = "Command template for spawning bare VT windows";
    };
    wmClass = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = "X window class, as stated by WM_CLASS property";
    };
    runtimeInputs = mkOption {
      type = types.listOf types.package;
      default = [ ];
      description = "Runtime inputs for selected application";
    };
  };
}
