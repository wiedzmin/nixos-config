{ config, lib, ... }:
with config.ide.emacs;

{
  consultDirProjectListFunction = lib.optionalString (navigation.projects.backend == "project") "consult-dir-project-dirs" +
    lib.optionalString (navigation.projects.backend == "projectile") "consult-dir-projectile-dirs";
}
