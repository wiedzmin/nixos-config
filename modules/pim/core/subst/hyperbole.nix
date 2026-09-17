{ config, ... }:

let
  user = config.attributes.mainUser.name;
in
{
  # TODO: consider parameterizing further
  hyperboleLoadPath = "/home/${user}/workspace/repos/git.savannah.gnu.org/git/hyperbole";
}
