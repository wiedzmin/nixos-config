{pkgs, stdenv, ...}:

let
  solidity-flattener = pkgs.pythonPackages.buildPythonPackage rec {
    version = "master";
    pname = "solidity-flattener";

    src = pkgs.fetchFromGitHub {
      owner = "zaytsevand";
      repo = "solidity-flattener";
      rev = "21c780e1a0812373f4e27c0ee44f681c812eb8cb";
      sha256 = "1mynahpyqf2d3lnghxdmhlmz94xs2nrzma74qv027haq2q61g3ff";
    };

    # Test settings are missing
    doCheck = false;

    meta = with stdenv.lib; {
      description = "A python utility to flatten Solidity code with imports into a single file";
      homepage = https://github.com/zaytsevand/solidity-flattener;
      license = licenses.bsd2;
    };
  };
in
{
  options = {};
  config = {
    nixpkgs.config.packageOverrides = super: {
      inherit solidity-flattener;
    };
  };
}
