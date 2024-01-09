{ config, ... }:

{
  fallbackBrowser = appCmdFull config.attributes.browser.fallback.traits;
}
