{ config, pkgs, lib, ... }:
with import ../../../../pkgs/util.nix { inherit config pkgs lib; };
with import ../../const.nix { inherit config pkgs; };
let
  maxLineLength = 120;
  indentWidth = 4;
  excludes = [ ".git" "__pycache__" ];
  ignoredCodes = [ "C901" "E124" "E128" "E201" "E203" "E211" "E251" "W503" "W504" ];
in {
  home-manager.users."${userName}" = {
    home.file = {
      ".pylintrc".text = genIni { # see https://github.com/PyCQA/pylint/blob/master/pylintrc for reference
          "MASTER" = {
              ignore = "CVS";
              persistent = "yes";
              jobs = "1";
              unsafe-load-any-extension = "no";
              optimize-ast = "no";
          };
          "MESSAGES CONTROL" = {
              disable = lib.concatStringsSep "," [
                  "apply-builtin"
                  "backtick"
                  "basestring-builtin"
                  "buffer-builtin"
                  "cmp-builtin"
                  "cmp-method"
                  "coerce-builtin"
                  "coerce-method"
                  "delslice-method"
                  "dict-iter-method"
                  "dict-view-method"
                  "execfile-builtin"
                  "file-builtin"
                  "filter-builtin-not-iterating"
                  "getslice-method"
                  "hex-method"
                  "import-star-module-level"
                  "indexing-exception"
                  "input-builtin"
                  "intern-builtin"
                  "long-builtin"
                  "long-suffix"
                  "map-builtin-not-iterating"
                  "metaclass-assignment"
                  "next-method-called"
                  "no-absolute-import"
                  "nonzero-method"
                  "oct-method"
                  "old-division"
                  "old-ne-operator"
                  "old-octal-literal"
                  "old-raise-syntax"
                  "parameter-unpacking"
                  "print-statement"
                  "raising-string"
                  "range-builtin-not-iterating"
                  "raw_input-builtin"
                  "reduce-builtin"
                  "reload-builtin"
                  "round-builtin"
                  "setslice-method"
                  "standarderror-builtin"
                  "suppressed-message"
                  "unichr-builtin"
                  "unicode-builtin"
                  "unpacking-in-except"
                  "useless-suppression"
                  "using-cmp-argument"
                  "xrange-builtin"
                  "zip-builtin-not-iterating"
              ];
          };
          "REPORTS" = {
              output-format = "text";
              files-output = "no";
              reports = "yes";
              evaluation = "10.0 - ((float(5 * error + warning + refactor + convention) / statement) * 10)";
          };
          "VARIABLES" = {
              init-import = "no";
              dummy-variables-rgx = "_$|dummy";
              callbacks = "cb_,_cb";
          };
          "TYPECHECK" = {
              ignore-mixin-members = "yes";
          };
          "SPELLING" = {
              spelling-store-unknown-words = "no";
          };
          "SIMILARITIES" = {
              min-similarity-lines = "4";
              ignore-comments = "yes";
              ignore-docstrings = "yes";
              ignore-imports = "no";
          };
          "MISCELLANEOUS" = {
              notes = lib.concatStringsSep "," [
                  "FIXME"
                  "XXX"
                  "TODO"
              ];
          };
          "FORMAT" = {
              max-line-length = builtins.toString maxLineLength;
              ignore-long-lines = "^\s*(# )?<?https?://\S+>?$";
              single-line-if-stmt = "no";
              no-space-check = lib.concatStringsSep "," [
                  "trailing-comma"
                  "dict-separator"
              ];
              max-module-lines = "1000";
              indent-string="'    '";
              indent-after-paren = "4";
          };
          "LOGGING" = {
              logging-modules = "logging";
          };
          "BASIC" = {
              bad-functions = lib.concatStringsSep "," [
                  "map"
                  "filter"
                  "input"
              ];
              good-names = lib.concatStringsSep "," [
                  "map"
                  "filter"
                  "input"
                  "i"
                  "j"
                  "k"
                  "ex"
                  "Run"
                  "_"
              ];
              bad-names = lib.concatStringsSep "," [
                  "foo"
                  "bar"
                  "baz"
                  "toto"
                  "tutu"
                  "tata"
              ];
              include-naming-hint = "no";
              function-rgx = "[a-z_][a-z0-9_]{2,30}$";
              function-name-hint = "[a-z_][a-z0-9_]{2,30}$";
              variable-rgx = "[a-z_][a-z0-9_]{2,30}$";
              variable-name-hint = "[a-z_][a-z0-9_]{2,30}$";
              const-rgx = "(([A-Z_][A-Z0-9_]*)|(__.*__))$";
              const-name-hint = "(([A-Z_][A-Z0-9_]*)|(__.*__))$";
              attr-rgx = "[a-z_][a-z0-9_]{2,30}$";
              attr-name-hint = "[a-z_][a-z0-9_]{2,30}$";
              argument-rgx = "[a-z_][a-z0-9_]{2,30}$";
              argument-name-hint = "[a-z_][a-z0-9_]{2,30}$";
              class-attribute-rgx = "([A-Za-z_][A-Za-z0-9_]{2,30}|(__.*__))$";
              class-attribute-name-hint = "([A-Za-z_][A-Za-z0-9_]{2,30}|(__.*__))$";
              inlinevar-rgx = "[A-Za-z_][A-Za-z0-9_]*$";
              inlinevar-name-hint = "[A-Za-z_][A-Za-z0-9_]*$";
              class-rgx = "[A-Z_][a-zA-Z0-9]+$";
              class-name-hint = "[A-Z_][a-zA-Z0-9]+$";
              module-rgx = "(([a-z_][a-z0-9_]*)|([A-Z][a-zA-Z0-9]+))$";
              module-name-hint = "(([a-z_][a-z0-9_]*)|([A-Z][a-zA-Z0-9]+))$";
              method-rgx = "[a-z_][a-z0-9_]{2,30}$";
              method-name-hint = "[a-z_][a-z0-9_]{2,30}$";
              no-docstring-rgx = "^_";
              docstring-min-length = "-1";
          };
          "ELIF" = {
              max-nested-blocks = "5";
          };
          "DESIGN" = {
              max-args = "5";
              ignored-argument-names = "_.*";
              max-locals = "15";
              max-returns = "6";
              max-branches = "12";
              max-statements = "50";
              max-parents = "7";
              max-attributes = "7";
              min-public-methods = "2";
              max-public-methods = "20";
              max-bool-expr = "5";
          };
          "IMPORTS" = {
              deprecated-modules = lib.concatStringsSep "," [
                  "regsub"
                  "TERMIOS"
                  "Bastion"
                  "rexec"
              ];
          };
          "CLASSES" = {
              defining-attr-methods = lib.concatStringsSep "," [
                  "__init__"
                  "__new__"
                  "setUp"
              ];
              valid-classmethod-first-arg = "cls";
              valid-metaclass-classmethod-first-arg = "mcs";
              exclude-protected = lib.concatStringsSep "," [
                  "_asdict"
                  "_fields"
                  "_replace"
                  "_source"
                  "_make"
              ];
          };
          "EXCEPTIONS" = {
              overgeneral-exceptions = lib.concatStringsSep "," [
                  "Exception"
              ];
          };
      };
      # ".pylintrc".text = genIni
      ".isort.cfg".text = genIni {
          settings = {
              line_length = builtins.toString maxLineLength;
              indent = "'    '";
              multi_line_output = "2";
              sections = lib.concatStringsSep "," [
                  "FUTURE"
                  "STDLIB"
                  "FIRSTPARTY"
                  "THIRDPARTY"
                  "LOCALFOLDER"
              ];
              length_sort = "0";
              forced_separate = lib.concatStringsSep "," [
                  "django.contrib"
                  "django.utils"
              ];
              default_section = "FIRSTPARTY";
          };
      };
    };
    xdg.configFile."flake8".text = genIni {
        flake8 = {
            max-line-length = builtins.toString maxLineLength;
            exclude = lib.concatStringsSep "," excludes;
            ignore = lib.concatStringsSep ", " ignoredCodes;
        };
    };
    xdg.configFile."pycodestyle".text = genIni {
        pycodestyle = {
            exclude = lib.concatStringsSep "," excludes;
            max-line-length = builtins.toString maxLineLength;
            count = "False";
            ignore = lib.concatStringsSep ", " ignoredCodes;
            statistics = "True";
        };
    };
    xdg.configFile."yapf/style".text = genIni {
        style = {
            based_on_style = "pep8";
            align_closing_bracket_with_visual_indent = "True";
            allow_split_before_default_or_named_assigns = "True";
            arithmetic_precedence_indication = "True";
            blank_line_before_nested_class_or_def = "True";
            blank_lines_around_top_level_definition = "2";
            coalesce_brackets = "True";
            column_limit = builtins.toString maxLineLength;
            each_dict_entry_on_separate_line = "True";
            indent_width = builtins.toString indentWidth;
            spaces_before_comment = "2";
            split_before_logical_operator = "True";
            split_before_named_assigns = "True";
            split_complex_comprehension = "True";
            split_before_first_argument = "True";
        };
    };
  };
}
