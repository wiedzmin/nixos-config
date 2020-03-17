let
  deps = import ../../../nix/sources.nix;
  nixpkgs-pinned-02_08_19 = import deps.nixpkgs-pinned-02_08_19 { config.allowUnfree = true; };
  nixpkgs-pinned-05_12_19 = import deps.nixpkgs-pinned-05_12_19 { config.allowUnfree = true; };
in { config, lib, pkgs, ... }:
with import ../../util.nix { inherit config lib pkgs; };
with lib;

let cfg = config.tools.dbms;
in {
  options = {
    tools.dbms = {
      postgresql.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable PostgreSQL helper tools.";
      };
      mysql.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable MySQL helper tools.";
      };
      sqlite.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable Sqlite helper tools.";
      };
      misc.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable misc helper tools.";
      };
      cli.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable job dbms connectivity.";
      };
      cli.meta = mkOption {
        type = types.attrsOf types.attrs;
        default = { };
        description = "Job dbms metadata.";
      };
      xmonad.enable = mkOption {
        type = types.bool;
        default = false;
        description = "Whether to enable XMonad keybindings.";
      };
    };
  };

  config = mkMerge [
    (mkIf cfg.postgresql.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [ pgcenter nixpkgs-pinned-05_12_19.pgcli ];
      };
    })
    (mkIf cfg.mysql.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [ nixpkgs-pinned-05_12_19.mycli ];
      };
    })
    (mkIf cfg.sqlite.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [
          sqlitebrowser
          nixpkgs-pinned-02_08_19.litecli # TODO: shell automation: skim for selecting db file, you get the idea
        ];
      };
    })
    (mkIf cfg.misc.enable {
      home-manager.users."${config.attributes.mainUser.name}" = {
        home.packages = with pkgs; [ nixpkgs-pinned-05_12_19.nodePackages.elasticdump ];
      };
    })
    (mkIf (cfg.cli.enable && cfg.xmonad.enable) {
      nixpkgs.config.packageOverrides = _: rec {
        dbms = writePythonScriptWithPythonPackages "dbms" [
          pkgs.python3Packages.dmenu-python
          pkgs.python3Packages.redis
          pkgs.python3Packages.notify2
          pkgs.vpnctl
        ] (builtins.readFile
          (pkgs.substituteAll ((import ../subst.nix { inherit config pkgs lib; }) // { src = ./dbms.py; })));
      };
      custom.housekeeping.metadataCacheInstructions = ''
        ${pkgs.redis}/bin/redis-cli set misc/dbms_meta ${lib.strings.escapeNixString (builtins.toJSON cfg.cli.meta)}
      '';
      wm.xmonad.keybindings = { "M-r d" = ''spawn "${pkgs.dbms}/bin/dbms" >> showWSOnProperScreen "shell"''; };
    })
  ];
}
