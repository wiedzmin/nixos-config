{config, pkgs, lib, ...}:
with import ../../const.nix {inherit config pkgs;};
{
    home-manager.users.alex3rd = {
        programs.vscode = {
            enable = true;
            userSettings = {
                "update.channel" = "none";
                "[nix]"."editor.tabSize" = 4;
                "workbench.iconTheme" = "vscode-icons";
                "workbench.colorTheme" = "Zenburn";
                "editor.suggestSelection" = "first";
                "python.jediEnabled" = false;
                "editor.fontSize" = 11;
                "editor.fontFamily" = "Iosevka";
                "editor.fontWeight" = "Bold";
                "python.venvPath" = "/home/${userName}/.virtualenvs";
                "editor.cursorBlinking" = "smooth";
            };
            extensions = [
                pkgs.vscode-extensions.bbenoist.Nix
            ] ++ pkgs.vscode-utils.extensionsFromVscodeMarketplace [
                {
                    name = "python";
                    publisher = "ms-python";
                    version = "2019.4.11987";
                    sha256 = "0iq4pbz4sy7730ap9kbv8b5ncb3jbxpf0ibsyjf3rs6j6jwvdv1v";
                }
                {
                    name = "vscodeintellicode";
                    publisher = "VisualStudioExptTeam";
                    version = "1.1.5";
                    sha256 = "12d6s7kfk4102gnfb2rk1sb00p3ql9xy86jykz562jlb20frp9s1";
                }
                {
                    name = "autodocstring";
                    publisher = "njpwerner";
                    version = "0.3.0";
                    sha256 = "1mlafwzag4l42y070ssgqi34zgg7khb4lc45ycnz0a13ql0f9yx6";
                }
                {
                    name = "trailing-spaces";
                    publisher = "shardulm94";
                    version = "0.3.1";
                    sha256 = "0h30zmg5rq7cv7kjdr5yzqkkc1bs20d72yz9rjqag32gwf46s8b8";
                }
                {
                    name = "gitlens";
                    publisher = "eamodio";
                    version = "9.6.3";
                    sha256 = "0psgvlf3945724l2lf3znr4xlmsk0zcrig48lap2bck43lr7xxfw";
                }
                {
                    name = "Bookmarks";
                    publisher = "alefragnani";
                    version = "10.4.3";
                    sha256 = "1y6y17zcfx56dqnki7yz4awl5b0xwajrlfg2jbwcpwpsi0njz46q";
                }
                {
                    name = "vscode-importmagic";
                    publisher = "brainfit";
                    version = "0.1.3";
                    sha256 = "0vpdcd3z6z0bwpd2832ndgx76fng8kbl6fddi64l8qy7agj2znn6";
                }
                {
                    name = "jinja";
                    publisher = "wholroyd";
                    version = "0.0.8";
                    sha256 = "1ln9gly5bb7nvbziilnay4q448h9npdh7sd9xy277122h0qawkci";
                }
                {
                    name = "python-preview";
                    publisher = "dongli";
                    version = "0.0.4";
                    sha256 = "08z0r8v5nkhg1mx7846p7s8mdnhx7w5ijbmbxav09yicxld04xz7";
                }
                {
                    name = "vscode-django";
                    publisher = "batisteo";
                    version = "0.19.0";
                    sha256 = "1bn9rkdqn9w23yn1sv6rivqirfvmn5xi7sfmj6d4z6qwy2cnwxfj";
                }
                {
                    name = "arepl";
                    publisher = "almenon";
                    version = "1.0.13";
                    sha256 = "1170hfrm6blfzjsk8n6w8l7yi9q5z4z89pgxfwkmkfdqmhpx1avn";
                }
                {
                    name = "vscode-python-test-adapter";
                    publisher = "LittleFoxTeam";
                    version = "0.3.4";
                    sha256 = "1k5y4w2qi0xm5a116kvpprlnpdididp76z4yzhn1xwzrmglnjcjg";
                }
                {
                    name = "vscode-gitignore-generator";
                    publisher = "piotrpalarz";
                    version = "1.0.1";
                    sha256 = "12l2gz73gni3n82dpiaq2sr8348437a6wnzyndaqdmxw5h26rhlb";
                }
                {
                    name = "githistory";
                    publisher = "donjayamanne";
                    version = "0.4.6";
                    sha256 = "1wj838iv1xg25a604j4mccdcqhjjcgpaaq6yhnng1ph0s16ypin1";
                }
                {
                    name = "project-manager";
                    publisher = "alefragnani";
                    version = "10.5.1";
                    sha256 = "1k8l5pyacpld9r76fqynpdx0zkzylvb5lickvxlnql2zb70cxk05";
                }
                {
                    name = "vscode-icons";
                    publisher = "vscode-icons-team";
                    version = "8.6.0";
                    sha256 = "0gcl43pdvkjnkdwc4zbl5xikjzij74m093p9hja4m36bmzvnixkp";
                }
                {
                    name = "django-intellisense";
                    publisher = "shamanu4";
                    version = "0.0.2";
                    sha256 = "146irhf1mfzawl4kbmx9zzs8rb9yvi6wr83mm8hhh3f0ihkdxdrn";
                }
                {
                    name = "jinjahtml";
                    publisher = "samuelcolvin";
                    version = "0.10.1";
                    sha256 = "0z3g3dd4ixrphf31jvacr5bsywhhnkzpdapgr3ncyr1cr19d9vbf";
                }
                {
                    name = "vscode-awesome-snippets";
                    publisher = "xindzju";
                    version = "0.2.1";
                    sha256 = "02v8ky8gavfw8gi7pf1amyzhmrmyvx8hw947q8ya9qbmxy1bdi7q";
                }
                {
                    name = "python-path";
                    publisher = "mgesbert";
                    version = "0.0.11";
                    sha256 = "06m2daywn234maicm4p9w1kz58d61fkvqjvcybkglkj91japj7mn";
                }
                {
                    name = "omnipascal";
                    publisher = "Wosi";
                    version = "0.17.1";
                    sha256 = "0wn322vwd0jhwqsh5fzdsiiywmrrx0i9fr2wrd6c5qxwkivhiic1";
                }
                {
                    name = "pascal";
                    publisher = "alefragnani";
                    version = "8.0.1";
                    sha256 = "13kla3wlqcgxqmln2qchag7j6z54ckzi0g5x3jggf050gvm16v5k";
                }
                {
                    name = "pascal-formatter";
                    publisher = "alefragnani";
                    version = "2.2.0";
                    sha256 = "1zz5zb2ma9m54hjy69pd9j4d3dq492g3vsz27jwvlzzzqwxvy30g";
                }
                {
                    name = "zenburn";
                    publisher = "ryanolsonx";
                    version = "1.0.1";
                    sha256 = "1gb159fkvadsnx5gb3l8pps9sa5xjcaqqiym952ar0yfhpfd0ziz";
                }
                {
                    name = "vscode-docker";
                    publisher = "PeterJausovec";
                    version = "0.6.1";
                    sha256 = "0clxy66qi5c3k5di5xsjm3vjib525xq89z1q2h3a5x5qwvbvd0mj";
                }
                {
                    name = "docker-compose";
                    publisher = "p1c2u";
                    version = "0.3.4";
                    sha256 = "1kpsxkridsvbhx7c7jlpyh4k0vhmr503fw2csh4vy3fc5f96x9ws";
                }
            ];
        };
    };
}
