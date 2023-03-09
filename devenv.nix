{ config, pkgs, inputs, ... }:

{
  env.PROJECTNAME = "nixos-config";

  scripts.hello.exec = "echo welcome to $PROJECTNAME";

  imports = [ inputs.nur.nixosModules.nur ];

  packages = with pkgs; with config.nur.repos; [
    cloc
    gitAndTools.git-crypt
    just
    nixUnstable
    vim
  ];

  enterShell = ''
    hello
  '';

  languages.lua.enable = true;
  languages.nix.enable = true;
  languages.python.enable = true;

  pre-commit.hooks = {
    deadnix.enable = true;
    nixpkgs-fmt.enable = true;
    shfmt.enable = true;
    typos.enable = true;
  };
}
