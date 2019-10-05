{ config, lib, ... }:

with lib;

{
  options.attributes = {
    mainUser = mkOption {
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
    testLines = mkOption {
      description = "testing lines merge";
      type = types.lines;
    };
  };
}
