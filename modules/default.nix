{ config, _class, inputs, lib, modulesPath, options, _prefix, specialArgs }: {
  imports = [
    ./attributes.nix

    ./appearance/colorschemes/dracula
    ./appearance/colorschemes/zenburn
    ./appearance/emacs
    ./appearance/fonts
    ./appearance/gtk
    ./appearance/wallpaper
    ./appearance/xresources
    ./browsers/chromium
    ./browsers/ext
    ./browsers/firefox
    ./browsers/nyxt
    ./browsers/qutebrowser
    ./completion/expansions
    ./completion/tabnine
    ./content/core
    ./content/ebooks
    ./content/images
    ./content/media
    ./content/misc
    ./content/screenshots
    ./controlcenter
    ./dev/ccpp
    ./dev/dbms/misc
    ./dev/dbms/mysql
    ./dev/dbms/pgsql
    ./dev/direnv
    ./dev/editorconfig
    ./dev/frontend
    ./dev/git/autofetch
    ./dev/git/autopush
    ./dev/vcs
    ./dev/git/core
    ./dev/git/forges
    ./dev/git/misc
    ./dev/git/navigation
    ./dev/git/savewip
    ./dev/golang
    ./dev/lisp
    ./dev/misc
    ./dev/ml
    ./dev/navigation/codesearch
    ./dev/navigation/projects
    ./dev/python
    ./emacs/completion
    ./emacs/core
    ./emacs/edit
    ./emacs/misc
    ./emacs/navigation
    ./email
    ./email/mbsync.nix
    ./gc
    ./history
    ./knowledgebase
    ./navigation/bookmarks
    ./networking/core
    ./networking/hosts
    ./networking/messengers
    ./networking/nmconnections
    ./networking/ssh
    ./networking/vpn
    ./networking/wireless
    ./nix/cachix
    ./nix/core
    ./nix/dev
    ./nix/navigation
    ./paperworks
    ./pim/core
    ./pim/orgmode
    ./pim/scheduling
    ./pim/timetracking
    ./security
    ./shell/alacritty
    ./shell/bookmarks
    ./shell/core
    ./shell/fish
    ./shell/kitty
    ./shell/prompts/liquid
    ./shell/prompts/ohmyzsh
    ./shell/prompts/powerline-go
    ./shell/prompts/starship
    ./shell/tools
    ./shell/twopanes
    ./shell/zsh
    ./virt/core
    ./virt/docker/core
    ./virt/docker/devdns
    ./virt/virtualbox
    ./wm
    ./wm/impl/awesome
    ./wm/impl/herbstluft
    ./wm/impl/i3
    ./wm/impl/qtile
    ./workstation/backups
    ./workstation/input/core
    ./workstation/input/mouse
    ./workstation/input/keyboard
    ./workstation/lockscreen
    ./workstation/performance
    ./workstation/power/auto-cpufreq
    ./workstation/power/battery
    ./workstation/power/mgmt
    ./workstation/randr
    ./workstation/sound
    ./workstation/systemtraits
    ./workstation/video/backlight
    ./workstation/video/opengl
    ./workstation/video/transparency
  ];
}
