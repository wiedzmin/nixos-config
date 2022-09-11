{ pkgs ? import <nixpkgs> { }, ... }:

with pkgs;
let
  nurpkgs = pkgs.nur.repos; # refer to packages as nurpkgs.<username>.<package>
  base = [ ansible codesearch ctmg gitAndTools.pre-commit gnumake ];
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
in
mkShell {
  buildInputs = base ++ git ++ [ ];
  shellHook = ''
    export ANSIBLE_RETRY_FILES_SAVE_PATH=`pwd`/.ansible
    export ANSIBLE_SSH_CONTROL_PATH_DIR=`pwd`/.ansible/cp
    export BECOME_PLUGIN_PATH=`pwd`/.ansible/plugins/become
    export COLLECTIONS_PATHS=`pwd`/.ansible/collections
    export DEFAULT_ACTION_PLUGIN_PATH=`pwd`/.ansible/plugins/action
    export DEFAULT_CACHE_PLUGIN_PATH=`pwd`/.ansible/plugins/cache
    export DEFAULT_CALLBACK_PLUGIN_PATH=`pwd`/.ansible/plugins/callback
    export DEFAULT_CLICONF_PLUGIN_PATH=`pwd`/.ansible/plugins/cliconf
    export DEFAULT_CONNECTION_PLUGIN_PATH=`pwd`/.ansible/plugins/connection
    export DEFAULT_FILTER_PLUGIN_PATH=`pwd`/.ansible/plugins/filter
    export DEFAULT_HTTPAPI_PLUGIN_PATH=`pwd`/.ansible/plugins/httpapi
    export DEFAULT_INVENTORY_PLUGIN_PATH=`pwd`/.ansible/plugins/inventory
    export DEFAULT_LOCAL_TMP=`pwd`/.ansible/tmp
    export DEFAULT_LOOKUP_PLUGIN_PATH=`pwd`/.ansible/plugins/lookup
    export DEFAULT_MODULE_PATH=`pwd`/.ansible/plugins/modules
    export DEFAULT_MODULE_UTILS_PATH=`pwd`/.ansible/plugins/module_utils
    export DEFAULT_NETCONF_PLUGIN_PATH=`pwd`/.ansible/plugins/netconf
    export DEFAULT_ROLES_PATH=`pwd`/.ansible/roles
    export DEFAULT_STRATEGY_PLUGIN_PATH=`pwd`/.ansible/plugins/strategy
    export DEFAULT_TERMINAL_PLUGIN_PATH=`pwd`/.ansible/plugins/terminal
    export DEFAULT_TEST_PLUGIN_PATH=`pwd`/.ansible/plugins/test
    export DEFAULT_VARS_PLUGIN_PATH=`pwd`/.ansible/plugins/vars
    export DOC_FRAGMENT_PLUGIN_PATH=`pwd`/.ansible/plugins/doc_fragments
    export GALAXY_TOKEN_PATH=`pwd`/.ansible/galaxy_token
    export PERSISTENT_CONTROL_PATH_DIR=`pwd`/.ansible/pc
  '';
}
