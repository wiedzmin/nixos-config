{ config, inputs, lib, pkgs, ... }:

rec {
  lockScreenCommand = config.workstation.lockscreen.command.lock;
}
