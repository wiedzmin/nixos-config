{ config, ... }:

{
  fallbackBrowser = appBrowserCmd config.attributes.browser.fallback.traits;
}
