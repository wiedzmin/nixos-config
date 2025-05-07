{ config, lib, ... }:
with config.ide.emacs;

{
  projectRootSexp = lib.optionalString (navigation.projects.backend == "project") "(project-root (project-current))" +
    lib.optionalString (navigation.projects.backend == "projectile") "(projectile-project-root)";
  projectBackendRequire = lib.optionalString (navigation.projects.backend == "project") "(require 'project)" +
    lib.optionalString (navigation.projects.backend == "projectile") "(require 'projectile)";
}
