{ pkgs ? import <nixpkgs> { }, ... }:

# jupyterWithPackages = pkgs.jupyter.override {
#   definitions = {
#     python3 = let env = (pkgs.python3.withPackages (ps: with ps; [ ipykernel ] ++ pythonLibs));
#     in {
#       displayName = "Python 3 Notebook";
#       argv = [ "${env.interpreter}" "-m" "ipykernel_launcher" "-f" "{connection_file}" ];
#       language = "python";
#       logo32 = "${env.sitePackages}/ipykernel/resources/logo-32x32.png";
#       logo64 = "${env.sitePackages}/ipykernel/resources/logo-64x64.png";
#     };
#   };
# };

# nurpkgs.wiedzmin.dephell produces "error: stack overflow (possible infinite recursion)" when building environment
# prospector - # FIXME: update version, is currently broken # TODO: review configuration https://github.com/PyCQA/prospector

with pkgs;
let
  pythonLibs = with pkgs; [ ];
  nurpkgs = pkgs.nur.repos; # refer to packages as nurpkgs.<username>.<package>
  base = [ codesearch docker_compose gitAndTools.pre-commit go-task watchman ];
  stats = [ cloc gource logtop sloccount tokei ];
  git = [
    git-quick-stats
    git-sizer
    gitAndTools.git-filter-repo
    gitAndTools.git-machete
    gitAndTools.git-reparent
    gitAndTools.git-subset
    gitAndTools.git-trim
    gitstats
    gomp
  ];
  python = [
    python3Packages.black
    python3Packages.isort
    python3Packages.poetry
    python3Packages.pycodestyle
    python3Packages.pylint
    yapf
  ];
in
mkShell {
  buildInputs = base ++ stats ++ git ++ python ++ [ ];
  shellHook = ''
    [ -f "./.aux" ] && source ./.aux
  '';
}
