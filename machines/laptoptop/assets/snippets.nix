{ config, inputs, lib, pkgs, ... }:
with import ../../../modules/util.nix { inherit config inputs lib pkgs; };

let user = config.attributes.mainUser.name;
in rec {
  completion.snippets.entries = [
    {
      description = "vdi --> qcow2";
      language = "shell";
      code = "qemu-img convert -f vdi -O qcow2 vm-disk-name.vdi vm-disk-name.qcow2";
      tags = [ "virt" ];
    }
    {
      description = "write iso to flash drive";
      language = "shell";
      code = "sudo dd bs=4M if=/path/from/file.iso of=/dev/sdb1 conv=fdatasync";
      tags = [ "system" "fs" ];
    }
    {
      description = "batch `direnv allow`";
      language = "shell";
      code = "nix run nixpkgs.mr -c mr direnv";
      tags = [ "git" "automation" "batch" "direnv" ];
    }
    {
      description = "reload GPG agent metadata";
      language = "shell";
      code = "gpgconf --kill gpg-agent";
      tags = [ "security" "gpg" "services" ];
    }
    {
      description = "batch `git trim`";
      language = "shell";
      code = "nix run nixpkgs.mr -c mr trim";
      tags = [ "git" "automation" "batch" "cleanup" ];
    }
    {
      description = "checkout repo's main branch";
      language = "shell";
      code = "git checkout master";
      tags = [ "git" ];
    }
    {
      description = "nix prefetch github repo";
      language = "shell";
      code = "nix-prefetch-github user repo --rev foo";
      tags = [ "nix" "dev" "git" ];
    }
    {
      description = "convert doc to pdf";
      language = "shell";
      code = "libreoffice --headless --convert-to pdf inputfile.docx";
      tags = [ "office" "doc" "pdf" ];
    }
    {
      description = "list failed systemd services";
      language = "shell";
      code = "systemctl --user --state=failed";
      tags = [ "system" "services" ];
    }
    {
      description = "reset failed systemd services";
      language = "shell";
      code = "systemctl --user reset-failed";
      tags = [ "system" "services" ];
    }
    {
      description = "list new packages in Nixpkgs local git repo";
      language = "shell";
      code = "git log --pretty=oneline ORIG_HEAD..FETCH_HEAD | grep init | grep -v Merge";
      tags = [ "nixpkgs" "git" ];
    }
    {
      description = "trace filesystem events";
      language = "shell";
      code = "nix run nixpkgs.fatrace -c fatrace";
      tags = [ "monitoring" "fs" ];
    }
    {
      description = "list Nix store tree for current profile";
      language = "shell";
      code = "nix-store -q --tree ~/.nix-profile";
      tags = [ "nix" ];
    }
    {
      description = "export git stash as patch";
      language = "shell";
      code = "git stash show -p stash@{0} > stash0.patch";
      tags = [ "git" "patch" "export" ];
    }
    {
      description = "find same file";
      language = "shell";
      code = "find -L /where -samefile /what/to/find";
      tags = [ "search" ];
    }
    {
      description = "age-bound files content search";
      language = "shell";
      code = "fd --change-newer-than \"2019-05-22 10 = 00 = 00\" -x rg -l \"\" '{}'";
      tags = [ "search" ];
    }
    {
      description = "install GRUB loader";
      language = "shell";
      code = "sudo grub-install /dev/sda";
      tags = [ "grub" "system" ];
    }
    {
      description = "show global git config";
      language = "shell";
      code = "git config --global --list";
      tags = [ "git" ];
    }
    {
      description = "update ebooks store";
      language = "shell";
      code = "systemctl --user start update-ebooks.service";
      tags = [ "ebooks" "system" "service" ];
    }
    {
      description = "pre-commit for all";
      language = "shell";
      code = "pre-commit run --all-files";
      tags = [ "git" "automation" ];
    }
    {
      description = "shrug";
      code = "¯\\_(ツ)_/¯";
    }
    {
      description = "kata smile";
      code = "ツ";
    }
    {
      description = "tabnine semantic completion";
      code = "TabNine::sem";
    }
    {
      description = "run vim";
      code = "nix run nixpkgs.vim -c vim ";
    }
    {
      description = "week in seconds";
      code = "604800";
    }
    {
      description = "remove Yeganesh cache";
      code = "rm /home/${user}/.local/share/yeganesh/default";
    }
  ];
}
