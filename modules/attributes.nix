{ config, lib, ... }:

with lib;

{
  options.attributes = {
    mainUser = mkOption {
      description = "Main user to be granted various service-related rights to";
      type = types.str;
    };
  };
}
