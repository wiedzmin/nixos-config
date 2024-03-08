{ config, pkgs, inputs, ... }:

{
  env = {
    PROJECTNAME = "nixos-config";
    GREEN = "\\033[0;32m";
    NC = "\\033[0m"; # No Color
  };

  scripts.hello.exec = ''echo -e "''${GREEN}welcome to $PROJECTNAME''${NC}"'';

  imports = [ inputs.nur.nixosModules.nur ];

  packages = with pkgs; with config.nur.repos; [
    cloc
    gitFull
    gitAndTools.git-crypt
    just
    tagref
    vim
    wiedzmin.pystdlib
  ];

  enterShell = ''
    hello
  '';

  difftastic.enable = true;

  languages.lua.enable = true;
  languages.nix.enable = true;
  languages.python = {
    enable = true;
    package = pkgs.python311;
  };

  pre-commit.hooks = {
    deadnix.enable = true;
    nixpkgs-fmt.enable = true;
    shfmt.enable = true;
    typos.enable = true;
  };
}
