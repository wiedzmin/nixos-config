{ config, ... }:

rec {
  fallbackBrowser = config.attributes.browser.fallback.cmd;
}
