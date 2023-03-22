{ config, lib, pkgs, ... }:
with pkgs.unstable.commonutils;

let
  user = config.attributes.mainUser.name;
  yaml = pkgs.formats.yaml { };
in
rec {
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
      description = "show scoped git config";
      language = "shell";
      code = "git config --list --show-origin --show-scope";
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
    {
      description = "jwt certificates";
      code = "step-cli";
      tags = [ "jwt" "certificates" "ssl" ];
    }
    {
      description = "golint alternative";
      code = "revive";
      tags = [ "golang" "lint" ];
    }
    {
      description = "step inspect jwt";
      code = "step-cli crypto jwt inspect --insecure";
    }
    {
      description = "pandoc md to pdf";
      code = "pandoc --self-contained -t html -o bosse.pdf bosse.md";
    }
    {
      description = "multiline replace";
      code = "fd network-policies.yml.j2 deploymentsets -x perl -0777 -i -pe 's/(?<match>    - podSelector:\\n        matchLabels:\\n          deploymentSet: svhb-device-gateway\\n          podName: svhb-device-gateway)/$+{match}\\n    # TODO: This selector can be removed once the new, split-up svhb deploymentSets (SHCB-889) have found their way to prod\\n    - podSelector:\\n        matchLabels:\\n          deploymentSet: svhb\\n          podName: svhb-device-gateway/g' {}";
    }
    {
      description = "ssl cert text";
      code = "openssl s_client -showcerts -servername $server -connect $server:443 </dev/null  | openssl x509 -inform pem -noout -text";
    }
    {
      description = "ssl cert dates";
      code = "openssl s_client -connect example.com:443 2>/dev/null |openssl x509 -dates -noout\"";
    }
    {
      description = "ssh forward";
      code = "ssh -L localhost:9200:192.168.16.2:9200 root@10.1.10.70";
    }
    {
      description = "ssh jump forward";
      code = "ssh -v -L 8080:localhost:80 user@local.virtual -J user@jump:1077";
    }
    {
      description = "gpg recipients";
      code = "gpg --list-only --no-default-keyring --secret-keyring /dev/null";
    }
    {
      description = "emacs unfreeze";
      code = "pgrep -x emacs | xargs kill -SIGUSR2";
    }
    {
      description = "interactive sed";
      code = "fd <files> | sad <pattern> <replacement> | delta";
    }
    {
      description = "restart gpg agent";
      code = "gpgconf --kill gpg-agent; # echo \"UPDATESTARTUPTTY\" | gpg-connect-agent > /dev/null 2>&1";
    }
    {
      description = "java view jar content";
      code = "jar tf";
    }
    {
      description = "shebang";
      code = "#!/usr/bin/env bash";
    }
    {
      description = "git prune branches";
      code = "git prune-remote; git prune-local";
    }
    {
      description = "colorpicker";
      code = "grim -g \"$(slurp -p)\" -t ppm - | convert - -format '%[pixel:p{0,0}]' txt:- | tail -n 1 | cut -d ' ' -f 4 | wl-copy";
    }
    {
      description = "collect tag attrs over html file";
      code = ''xidel data.html --extract '//img[@class="imageclass"]/@src' --output-format json > links.txt'';
    }
    {
      description = "show hidden nix GC roots";
      code = ''nix-store --gc --print-roots | cut -d' ' -f1 | uniq |grep -v /proc | grep -v { | grep -v /run | grep alex3rd | grep direnv'';
    }
    {
      description = "distinct count of files containing the given search token(s)";
      code = ''rg -l "" | cut -f1,2 -d/ | sort | uniq | wc -l'';
    }
  ];
  home-manager.users."${user}" = lib.optionals (config.completion.expansions.enable) {
    xdg.configFile."espanso/match/personal.yml".source = yaml.generate "espanso-personal.yml" {
      name = "personal";
      parent = "default";
      matches = [
        {
          trigger = ":emp";
          replace = "${config.attributes.mainUser.email}";
        }
      ];
    };
  };
}
