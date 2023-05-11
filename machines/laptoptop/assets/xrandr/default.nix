{ ... }: {
  imports = [
    ./docked_home.nix
    ./docked_home_dualhead.nix
    ./docked_home_dualhead_swapped.nix
    ./layouts/layout_one_exthead_internal_atright.nix
    ./layouts/layout_two_extheads_internal_atright.nix
    ./layouts/layout_two_extheads_internal_downright.nix
    ./layouts/layout_two_extheads_internal_downright_aligned.nix
    ./mobile.nix
  ];
}
