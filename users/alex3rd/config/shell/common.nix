{ config, pkgs, lib, ... }:
with import ../../const.nix { inherit config pkgs; };
with import ../../secrets/const.nix { inherit config pkgs lib; }; {
  imports = [ ../../secrets/job.nix ];

  home-manager.users."${userName}" = {
    programs.htop = {
      enable = true;
      fields = [ "USER" "PRIORITY" "NICE" "M_SIZE" "STATE" "PERCENT_CPU" "PERCENT_MEM" "TIME" "COMM" ];
      meters.left = [ "AllCPUs" "Memory" ];
      colorScheme = 0;
      detailedCpuTime = true;
    };
    programs.command-not-found.enable = true;
    programs.lesspipe.enable = true;
    programs.man.enable = true;
    programs.info.enable = true;
    programs.skim = {
      enable = true;
      historyWidgetOptions = [ "--exact" ];
      defaultOptions = [ "--height 40%" "--prompt ⟫" ];
      fileWidgetCommand = "${pkgs.fd}/bin/fd --type f";
      fileWidgetOptions = [ "--preview 'head {}'" ];
      changeDirWidgetCommand = "${pkgs.fd}/bin/fd --type d";
      changeDirWidgetOptions = [ "--preview 'tree -C {} | head -200'" ];
      enableZshIntegration = true;
    };
    programs.direnv = {
      enable = true;
      enableZshIntegration = true;
    };
    programs.z-lua = {
      enable = true;
      enableZshIntegration = true;
      options = [ "fzf" "enhanced" "once" ];
    };
  };
}
