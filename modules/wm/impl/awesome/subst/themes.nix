{ config, ... }:

{
  mainUserName = config.attributes.mainUser.name;
  wmFontSimple = config.wmCommon.fonts.simple;
}
