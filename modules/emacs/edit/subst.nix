{ config, inputs, ... }:
with config.ide.emacs;

{
  emacsEasyKillExtrasPath = inputs.emacs-easy-kill-extras;
}
