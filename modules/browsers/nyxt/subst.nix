{ config, inputs, lib, pkgs, ... }:

rec {
  fallbackBrowser = config.attributes.browser.fallback.cmd;
}
