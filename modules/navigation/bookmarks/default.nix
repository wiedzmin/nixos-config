{ config, inputs, lib, pkgs, ... }:
with import ../../util.nix { inherit config inputs lib pkgs; };
with lib;

let
  cfg = config.navigation.bookmarks;
  user = config.attributes.mainUser.name;
in {
  options = {
    navigation.bookmarks = {
      enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable bookmarking functionality";
      };
      separator.fields = mkOption {
        type = types.str;
        default = " | ";
        description = "Bookmarks field separator";
      };
      separator.tags = mkOption {
        type = types.str;
        default = ":";
        description = "Bookmarks tags separator";
      };
      entries = mkOption {
        type = types.attrs;
        default = { };
        description = "Bookmarks data";
      };
      workspaces.roots = mkOption {
        type = types.attrs;
        default = { };
        description = "Various workspace roots meta";
      };
      workspaces.globalRoot = mkOption {
        type = types.str;
        default = "";
        description = "Global workspace root";
      };
      emacs.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable emacs setup";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.enable {
      assertions = [{
        assertion = config.workstation.systemtraits.enable;
        message = "navigation/bookmarks: must enable systemtraits maintainence.";
      }];

      home-manager.users."${user}" = lib.optionalAttrs cfg.emacs.enable {
        home.activation.emacsKnownProjects = {
          after = [ "linkGeneration" ];
          before = [ ];
          data = let
            uid = config.attributes.mainUser.ID;
            emacsPkg = config.ide.emacs.core.package;
          in emacsCmd uid emacsPkg
            "(mapcar (lambda (p) (projectile-add-known-project p)) (list ${
              builtins.concatStringsSep " " (localEmacsBookmarks cfg.entries)
            }))";
        };
      };

      workstation.systemtraits.instructions = with config.navigation.bookmarks; ''
        ${pkgs.redis}/bin/redis-cli set nav/bookmarks ${
          lib.strings.escapeNixString
            (builtins.toJSON (localBookmarksKeyMeta cfg.entries separator.fields separator.tags))
        }
      '';
    })
  ];
}
