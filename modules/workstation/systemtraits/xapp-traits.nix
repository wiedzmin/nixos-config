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
    command.parameters = mkOption {
      type = types.str;
      default = "";
      description = "X application command parameters";
    };
    wmClass = mkOption {
      type = types.listOf types.str;
      default = [ ];
      description = "X window class, as stated by WM_CLASS property";
    };
    suspensionRule = mkOption {
      type = types.attrs;
      default = { };
      description = "Rule for XSuspender, mostly for resource hogs";
    };
  };
}
