{ config, pkgs, lib, ... }:
with import ../../../../../pkgs/util.nix { inherit lib config pkgs; };
with import ../../../const.nix { inherit config pkgs; };
with import ../../../secrets/const.nix { inherit config lib pkgs; };
let
  custom = import ../../../../../pkgs/custom pkgs config;
  userCustom = import ../../../custom pkgs config;
in rec {
  backlightAmount = 10;
  volumeAmount = 10;

  playerDeltaSeconds = 10;
  volumeDeltaFraction = 0.1;
  volumeDeltaPercents = 10;

  autorandrProfiles = "${custom.autorandr_profiles}/bin/autorandr_profiles";
  bookshelf = "${userCustom.bookshelf}/bin/bookshelf";
  bukuAdd = "${custom.buku_add}/bin/buku_add";
  currentSystemHash = "${custom.current_system_hash}/bin/current_system_hash";
  dbms = "${userCustom.dbms}/bin/dbms";
  dockerContainerTraits = "${custom.docker_containers_traits}/bin/docker_containers_traits";
  dockerShell = "${userCustom.docker_shell}/bin/docker_shell";
  dockerStacksInfo = "${userCustom.docker_stacks_info}/bin/docker_stacks_info";
  extraHostsTraits = "${userCustom.extra_hosts_traits}/bin/extra_hosts_traits";
  jogEmacs = "${pkgs.procps}/bin/pkill -SIGUSR2 emacs";
  lockScreen = "${pkgs.i3lock-color}/bin/i3lock-color -c 232729 && ${pkgs.xorg.xset}/bin/xset dpms force off";
  mergeXresources = "${pkgs.xorg.xrdb}/bin/xrdb -merge $HOME/.Xresources";
  mountNasVolume = "${userCustom.mount_nas_volume}/bin/mount_nas_volume";
  remoteDockerLogs = "${userCustom.remote_docker_logs}/bin/remote_docker_logs";
  rofiCombiRun = "${pkgs.rofi}/bin/rofi -combi-modi drun,run -show combi -modi combi";
  rofiPass = "${pkgs.rofi-pass}/bin/rofi-pass";
  rofiSsh = "${pkgs.rofi}/bin/rofi -show ssh";
  rofiWindow = "${pkgs.rofi}/bin/rofi -show window";
  screenshotActiveWindow = "${custom.screenshot_active_window}/bin/screenshot_active_window";
  screenshotFull = "${custom.screenshot_full}/bin/screenshot_full";
  screenshotRegion = "${custom.screenshot_region}/bin/screenshot_region";
  searchPrompt = "${userCustom.search_prompt}/bin/search_prompt";
  searchSelection = "${userCustom.search_selection}/bin/search_selection";
  serviceJournal = "${custom.services_journals}/bin/services_journals";
  sshCustomUser = "${custom.ssh_custom_user}/bin/ssh_custom_user";
  tmuxpSessions = "${custom.tmuxp_sessions}/bin/tmuxp_sessions";
  unmountNasVolume = "${userCustom.unmount_nas_volume}/bin/unmount_nas_volume";
  uptimeInfo = "${custom.uptime_info}/bin/uptime_info";
  webjumps = "${userCustom.webjumps}/bin/webjumps";
  xrandrForceMobile = "${pkgs.autorandr}/bin/autorandr --load mobile";

  brightnessDown = "${light} -U ${toString backlightAmount}";
  brightnessMax = "${light} -S 100";
  brightnessMin = "${light} -S 20";
  brightnessUp = "${light} -A ${toString backlightAmount}";
  light = "${pkgs.light}/bin/light";
  pactl = "${pkgs.pulseaudio}/bin/pactl";
  pctl = "${pkgs.playerctl}/bin/playerctl --all-players";
  pctlLowerVolume = "${pctl} volume ${builtins.toString volumeDeltaFraction}-";
  pctlRaiseVolume = "${pctl} volume ${builtins.toString volumeDeltaFraction}+";
  pctlSeekBackward = "${pctl} position ${builtins.toString playerDeltaSeconds}-";
  pctlSeekForward = "${pctl} position ${builtins.toString playerDeltaSeconds}+";
  sctlRestart = "${pkgs.systemd}/bin/systemctl restart";
  sctlStop = "${pkgs.systemd}/bin/systemctl stop";
  sctlUserRestart = "${pkgs.systemd}/bin/systemctl --user restart";
  sctlUserStop = "${pkgs.systemd}/bin/systemctl --user stop";
  wpaGui = "${pkgs.wpa_supplicant_gui}/bin/wpa_gui";

  personalVpnUp = "${sctlRestart} openvpn-${personalVpnName}.service";
  personalVpnDown = "${sctlStop} openvpn-${personalVpnName}.service";
  jobVpnUp = "${sctlRestart} openvpn-${jobVpnName}.service";
  jobVpnDown = "${sctlStop} openvpn-${jobVpnName}.service";

  fontDefault = "${fontMainName}:${fontMainWeightKeyword}=${fontMainWeight}:${fontMainSizeKeyword}=${fontSizeDzen}";
  fontTabbed = "${fontMainName}:${fontMainWeightKeyword}=${fontMainWeight}:${fontMainSizeKeyword}=${fontSizeDzen}";
  xmobar = "${pkgs.xmobar}/bin/xmobar";
}
