{ config, ... }:

{
  deaddFontFamily = config.wmCommon.fonts.deaddFamily;
  deaddForegroundNormal = config.controlcenter.lnc.theme.foregroundNormal;
  deaddForegroundCritical = config.controlcenter.lnc.theme.foregroundCritical;
  deaddBackgroundNormal = config.controlcenter.lnc.theme.backgroundNormal;
  deaddBackgroundCritical = config.controlcenter.lnc.theme.backgroundCritical;
  deaddForegroundNormalNC = config.controlcenter.lnc.theme.foregroundNormalNC;
  deaddForegroundCriticalNC = config.controlcenter.lnc.theme.foregroundCriticalNC;
  deaddBackgroundNormalNC = config.controlcenter.lnc.theme.backgroundNormalNC;
  deaddBackgroundCriticalNC = config.controlcenter.lnc.theme.backgroundCriticalNC;
}
