{ config, inputs, lib, pkgs, ... }:
with import ../../../modules/util.nix { inherit config inputs lib pkgs; };

let
  user = config.attributes.mainUser.name;
in
rec {
  home-manager.users."${user}" = {
    xdg.configFile."espanso/user/spellcheck.yml".text = ''
      name: spellcheck
      parent: default

      matches:
        - trigger: "подохдать"
          replace: "подождать"
          word: true
    '';
  };
}
