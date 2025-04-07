{ config, inputs, ... }:

{
  emacsGitMsgPrefixPath = inputs.emacs-git-msg-prefix;
  ghqRoot = config.navigation.bookmarks.workspaces.globalRoot;
}
