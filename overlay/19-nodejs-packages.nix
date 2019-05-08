self: super:

let
    nodejs = super."nodejs-10_x";
    nodeEnv = import ../pkgs/nixpkgs-channels/pkgs/development/node-packages/node-env.nix {
        inherit (super) stdenv python2 utillinux runCommand writeTextFile;
        inherit nodejs;
        libtool = if super.stdenv.isDarwin then super.darwin.cctools else null;
    };
    sources = {
      "abab-2.0.0" = {
          name = "abab";
          packageName = "abab";
          version = "2.0.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/abab/-/abab-2.0.0.tgz";
              sha512 = "sY5AXXVZv4Y1VACTtR11UJCPHHudgY5i26Qj5TypE6DKlIApbwb5uqhXcJ5UUGbvZNRh7EeIoW+LrJumBsKp7w==";
          };
      };
      "acorn-5.7.3" = {
          name = "acorn";
          packageName = "acorn";
          version = "5.7.3";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/acorn/-/acorn-5.7.3.tgz";
              sha512 = "T/zvzYRfbVojPWahDsE5evJdHb3oJoQfFbsrKM7w5Zcs++Tr257tia3BmMP8XYVjp1S9RZXQMh7gao96BlqZOw==";
          };
      };
      "acorn-6.1.1" = {
          name = "acorn";
          packageName = "acorn";
          version = "6.1.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/acorn/-/acorn-6.1.1.tgz";
              sha512 = "jPTiwtOxaHNaAPg/dmrJ/beuzLRnXtB0kQPQ8JpotKJgTB6rX6c8mlf315941pyjBSaPg8NHXS9fhP4u17DpGA==";
          };
      };
      "acorn-globals-4.3.2" = {
          name = "acorn-globals";
          packageName = "acorn-globals";
          version = "4.3.2";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/acorn-globals/-/acorn-globals-4.3.2.tgz";
              sha512 = "BbzvZhVtZP+Bs1J1HcwrQe8ycfO0wStkSGxuul3He3GkHOIZ6eTqOkPuw9IP1X3+IkOo4wiJmwkobzXYz4wewQ==";
          };
      };
      "acorn-walk-6.1.1" = {
          name = "acorn-walk";
          packageName = "acorn-walk";
          version = "6.1.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/acorn-walk/-/acorn-walk-6.1.1.tgz";
              sha512 = "OtUw6JUTgxA2QoqqmrmQ7F2NYqiBPi/L2jqHyFtllhOUvXYQXf0Z1CYUinIfyT4bTCGmrA7gX9FvHA81uzCoVw==";
          };
      };
      "ajv-6.10.0" = {
          name = "ajv";
          packageName = "ajv";
          version = "6.10.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/ajv/-/ajv-6.10.0.tgz";
              sha512 = "nffhOpkymDECQyR0mnsUtoCE8RlX38G0rYP+wgLWFyZuUyuuojSSvi/+euOiQBIn63whYwYVIIH1TvE3tu4OEg==";
          };
      };
      "ansi-regex-2.1.1" = {
          name = "ansi-regex";
          packageName = "ansi-regex";
          version = "2.1.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/ansi-regex/-/ansi-regex-2.1.1.tgz";
              sha1 = "c3b33ab5ee360d86e0e628f0468ae7ef27d654df";
          };
      };
      "aproba-1.2.0" = {
          name = "aproba";
          packageName = "aproba";
          version = "1.2.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/aproba/-/aproba-1.2.0.tgz";
              sha512 = "Y9J6ZjXtoYh8RnXVCMOU/ttDmk1aBjunq9vO0ta5x85WDQiQfUF9sIPBITdbiiIVcBo03Hi3jMxigBtsddlXRw==";
          };
      };
      "are-we-there-yet-1.1.5" = {
          name = "are-we-there-yet";
          packageName = "are-we-there-yet";
          version = "1.1.5";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/are-we-there-yet/-/are-we-there-yet-1.1.5.tgz";
              sha512 = "5hYdAkZlcG8tOLujVDTgCT+uPX0VnpAH28gWsLfzpXYm7wP6mp5Q/gYyR7YQ0cKVJcXJnl3j2kpBan13PtQf6w==";
          };
      };
      "array-equal-1.0.0" = {
          name = "array-equal";
          packageName = "array-equal";
          version = "1.0.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/array-equal/-/array-equal-1.0.0.tgz";
              sha1 = "8c2a5ef2472fd9ea742b04c77a75093ba2757c93";
          };
      };
      "asn1-0.2.4" = {
          name = "asn1";
          packageName = "asn1";
          version = "0.2.4";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/asn1/-/asn1-0.2.4.tgz";
              sha512 = "jxwzQpLQjSmWXgwaCZE9Nz+glAG01yF1QnWgbhGwHI5A6FRIEY6IVqtHhIepHqI7/kyEyQEagBC5mBEFlIYvdg==";
          };
      };
      "assert-plus-1.0.0" = {
          name = "assert-plus";
          packageName = "assert-plus";
          version = "1.0.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/assert-plus/-/assert-plus-1.0.0.tgz";
              sha1 = "f12e0f3c5d77b0b1cdd9146942e4e96c1e4dd525";
          };
      };
      "async-limiter-1.0.0" = {
          name = "async-limiter";
          packageName = "async-limiter";
          version = "1.0.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/async-limiter/-/async-limiter-1.0.0.tgz";
              sha512 = "jp/uFnooOiO+L211eZOoSyzpOITMXx1rBITauYykG3BRYPu8h0UcxsPNB04RR5vo4Tyz3+ay17tR6JVf9qzYWg==";
          };
      };
      "asynckit-0.4.0" = {
          name = "asynckit";
          packageName = "asynckit";
          version = "0.4.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/asynckit/-/asynckit-0.4.0.tgz";
              sha1 = "c79ed97f7f34cb8f2ba1bc9790bcc366474b4b79";
          };
      };
      "aws-sign2-0.7.0" = {
          name = "aws-sign2";
          packageName = "aws-sign2";
          version = "0.7.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/aws-sign2/-/aws-sign2-0.7.0.tgz";
              sha1 = "b46e890934a9591f2d2f6f86d7e6a9f1b3fe76a8";
          };
      };
      "aws4-1.8.0" = {
          name = "aws4";
          packageName = "aws4";
          version = "1.8.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/aws4/-/aws4-1.8.0.tgz";
              sha512 = "ReZxvNHIOv88FlT7rxcXIIC0fPt4KZqZbOlivyWtXLt8ESx84zd3kMC6iK5jVeS2qt+g7ftS7ye4fi06X5rtRQ==";
          };
      };
      "balanced-match-1.0.0" = {
          name = "balanced-match";
          packageName = "balanced-match";
          version = "1.0.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/balanced-match/-/balanced-match-1.0.0.tgz";
              sha1 = "89b4d199ab2bee49de164ea02b89ce462d71b767";
          };
      };
      "bcrypt-pbkdf-1.0.2" = {
          name = "bcrypt-pbkdf";
          packageName = "bcrypt-pbkdf";
          version = "1.0.2";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/bcrypt-pbkdf/-/bcrypt-pbkdf-1.0.2.tgz";
              sha1 = "a4301d389b6a43f9b67ff3ca11a3f6637e360e9e";
          };
      };
      "bl-1.2.2" = {
          name = "bl";
          packageName = "bl";
          version = "1.2.2";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/bl/-/bl-1.2.2.tgz";
              sha512 = "e8tQYnZodmebYDWGH7KMRvtzKXaJHx3BbilrgZCfvyLUYdKpK1t5PSPmpkny/SgiTSCnjfLW7v5rlONXVFkQEA==";
          };
      };
      "brace-expansion-1.1.11" = {
          name = "brace-expansion";
          packageName = "brace-expansion";
          version = "1.1.11";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/brace-expansion/-/brace-expansion-1.1.11.tgz";
              sha512 = "iCuPHDFgrHX7H2vEI/5xpz07zSHB00TpugqhmYtVmMO6518mCuRMoOYFldEBl0g187ufozdaHgWKcYFb61qGiA==";
          };
      };
      "browser-process-hrtime-0.1.3" = {
          name = "browser-process-hrtime";
          packageName = "browser-process-hrtime";
          version = "0.1.3";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/browser-process-hrtime/-/browser-process-hrtime-0.1.3.tgz";
              sha512 = "bRFnI4NnjO6cnyLmOV/7PVoDEMJChlcfN0z4s1YMBY989/SvlfMI1lgCnkFUs53e9gQF+w7qu7XdllSTiSl8Aw==";
          };
      };
      "buffer-alloc-1.2.0" = {
          name = "buffer-alloc";
          packageName = "buffer-alloc";
          version = "1.2.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/buffer-alloc/-/buffer-alloc-1.2.0.tgz";
              sha512 = "CFsHQgjtW1UChdXgbyJGtnm+O/uLQeZdtbDo8mfUgYXCHSM1wgrVxXm6bSyrUuErEb+4sYVGCzASBRot7zyrow==";
          };
      };
      "buffer-alloc-unsafe-1.1.0" = {
          name = "buffer-alloc-unsafe";
          packageName = "buffer-alloc-unsafe";
          version = "1.1.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/buffer-alloc-unsafe/-/buffer-alloc-unsafe-1.1.0.tgz";
              sha512 = "TEM2iMIEQdJ2yjPJoSIsldnleVaAk1oW3DBVUykyOLsEsFmEc9kn+SFFPz+gl54KQNxlDnAwCXosOS9Okx2xAg==";
          };
      };
      "buffer-fill-1.0.0" = {
          name = "buffer-fill";
          packageName = "buffer-fill";
          version = "1.0.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/buffer-fill/-/buffer-fill-1.0.0.tgz";
              sha1 = "f8f78b76789888ef39f205cd637f68e702122b2c";
          };
      };
      "caseless-0.12.0" = {
          name = "caseless";
          packageName = "caseless";
          version = "0.12.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/caseless/-/caseless-0.12.0.tgz";
              sha1 = "1b681c21ff84033c826543090689420d187151dc";
          };
      };
      "chownr-1.1.1" = {
          name = "chownr";
          packageName = "chownr";
          version = "1.1.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/chownr/-/chownr-1.1.1.tgz";
              sha512 = "j38EvO5+LHX84jlo6h4UzmOwi0UgW61WRyPtJz4qaadK5eY3BTS5TY/S1Stc3Uk2lIM6TPevAlULiEJwie860g==";
          };
      };
      "code-point-at-1.1.0" = {
          name = "code-point-at";
          packageName = "code-point-at";
          version = "1.1.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/code-point-at/-/code-point-at-1.1.0.tgz";
              sha1 = "0d070b4d043a5bea33a2f1a40e2edb3d9a4ccf77";
          };
      };
      "combined-stream-1.0.7" = {
          name = "combined-stream";
          packageName = "combined-stream";
          version = "1.0.7";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/combined-stream/-/combined-stream-1.0.7.tgz";
              sha512 = "brWl9y6vOB1xYPZcpZde3N9zDByXTosAeMDo4p1wzo6UMOX4vumB+TP1RZ76sfE6Md68Q0NJSrE/gbezd4Ul+w==";
          };
      };
      "concat-map-0.0.1" = {
          name = "concat-map";
          packageName = "concat-map";
          version = "0.0.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/concat-map/-/concat-map-0.0.1.tgz";
              sha1 = "d8a96bd77fd68df7793a73036a3ba0d5405d477b";
          };
      };
      "console-control-strings-1.1.0" = {
          name = "console-control-strings";
          packageName = "console-control-strings";
          version = "1.1.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/console-control-strings/-/console-control-strings-1.1.0.tgz";
              sha1 = "3d7cf4464db6446ea644bf4b39507f9851008e8e";
          };
      };
      "core-util-is-1.0.2" = {
          name = "core-util-is";
          packageName = "core-util-is";
          version = "1.0.2";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/core-util-is/-/core-util-is-1.0.2.tgz";
              sha1 = "b5fd54220aa2bc5ab57aab7140c940754503c1a7";
          };
      };
      "cssom-0.3.6" = {
          name = "cssom";
          packageName = "cssom";
          version = "0.3.6";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/cssom/-/cssom-0.3.6.tgz";
              sha512 = "DtUeseGk9/GBW0hl0vVPpU22iHL6YB5BUX7ml1hB+GMpo0NX5G4voX3kdWiMSEguFtcW3Vh3djqNF4aIe6ne0A==";
          };
      };
      "cssstyle-1.2.2" = {
          name = "cssstyle";
          packageName = "cssstyle";
          version = "1.2.2";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/cssstyle/-/cssstyle-1.2.2.tgz";
              sha512 = "43wY3kl1CVQSvL7wUY1qXkxVGkStjpkDmVjiIKX8R97uhajy8Bybay78uOtqvh7Q5GK75dNPfW0geWjE6qQQow==";
          };
      };
      "dashdash-1.14.1" = {
          name = "dashdash";
          packageName = "dashdash";
          version = "1.14.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/dashdash/-/dashdash-1.14.1.tgz";
              sha1 = "853cfa0f7cbe2fed5de20326b8dd581035f6e2f0";
          };
      };
      "data-urls-1.1.0" = {
          name = "data-urls";
          packageName = "data-urls";
          version = "1.1.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/data-urls/-/data-urls-1.1.0.tgz";
              sha512 = "YTWYI9se1P55u58gL5GkQHW4P6VJBJ5iBT+B5a7i2Tjadhv52paJG0qHX4A0OR6/t52odI64KP2YvFpkDOi3eQ==";
          };
      };
      "whatwg-url-7.0.0" = {
          name = "whatwg-url";
          packageName = "whatwg-url";
          version = "7.0.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/whatwg-url/-/whatwg-url-7.0.0.tgz";
              sha512 = "37GeVSIJ3kn1JgKyjiYNmSLP1yzbpb29jdmwBSgkD9h40/hyrR/OifpVUndji3tmwGgD8qpw7iQu3RSbCrBpsQ==";
          };
      };
      "decompress-response-3.3.0" = {
          name = "decompress-response";
          packageName = "decompress-response";
          version = "3.3.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/decompress-response/-/decompress-response-3.3.0.tgz";
              sha1 = "80a4dd323748384bfa248083622aedec982adff3";
          };
      };
      "deep-extend-0.6.0" = {
          name = "deep-extend";
          packageName = "deep-extend";
          version = "0.6.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/deep-extend/-/deep-extend-0.6.0.tgz";
              sha512 = "LOHxIOaPYdHlJRtCQfDIVZtfw/ufM8+rVj649RIHzcm/vGwQRXFt6OPqIFWsm2XEMrNIEtWR64sY1LEKD2vAOA==";
          };
      };
      "deep-is-0.1.3" = {
          name = "deep-is";
          packageName = "deep-is";
          version = "0.1.3";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/deep-is/-/deep-is-0.1.3.tgz";
              sha1 = "b369d6fb5dbc13eecf524f91b070feedc357cf34";
          };
      };
      "delayed-stream-1.0.0" = {
          name = "delayed-stream";
          packageName = "delayed-stream";
          version = "1.0.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/delayed-stream/-/delayed-stream-1.0.0.tgz";
              sha1 = "df3ae199acadfb7d440aaae0b29e2272b24ec619";
          };
      };
      "delegates-1.0.0" = {
          name = "delegates";
          packageName = "delegates";
          version = "1.0.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/delegates/-/delegates-1.0.0.tgz";
              sha1 = "84c6e159b81904fdca59a0ef44cd870d31250f9a";
          };
      };
      "detect-libc-1.0.3" = {
          name = "detect-libc";
          packageName = "detect-libc";
          version = "1.0.3";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/detect-libc/-/detect-libc-1.0.3.tgz";
              sha1 = "fa137c4bd698edf55cd5cd02ac559f91a4c4ba9b";
          };
      };
      "domexception-1.0.1" = {
          name = "domexception";
          packageName = "domexception";
          version = "1.0.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/domexception/-/domexception-1.0.1.tgz";
              sha512 = "raigMkn7CJNNo6Ihro1fzG7wr3fHuYVytzquZKX5n0yizGsTcYgzdIUwj1X9pK0VvjeihV+XiclP+DjwbsSKug==";
          };
      };
      "ecc-jsbn-0.1.2" = {
          name = "ecc-jsbn";
          packageName = "ecc-jsbn";
          version = "0.1.2";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/ecc-jsbn/-/ecc-jsbn-0.1.2.tgz";
              sha1 = "3a83a904e54353287874c564b7549386849a98c9";
          };
      };
      "end-of-stream-1.4.1" = {
          name = "end-of-stream";
          packageName = "end-of-stream";
          version = "1.4.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/end-of-stream/-/end-of-stream-1.4.1.tgz";
              sha512 = "1MkrZNvWTKCaigbn+W15elq2BB/L22nqrSY5DKlo3X6+vclJm8Bb5djXJBmEX6fS3+zCh/F4VBK5Z2KxJt4s2Q==";
          };
      };
      "escodegen-1.11.1" = {
          name = "escodegen";
          packageName = "escodegen";
          version = "1.11.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/escodegen/-/escodegen-1.11.1.tgz";
              sha512 = "JwiqFD9KdGVVpeuRa68yU3zZnBEOcPs0nKW7wZzXky8Z7tffdYUHbe11bPCV5jYlK6DVdKLWLm0f5I/QlL0Kmw==";
          };
      };
      "esprima-3.1.3" = {
          name = "esprima";
          packageName = "esprima";
          version = "3.1.3";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/esprima/-/esprima-3.1.3.tgz";
              sha1 = "fdca51cee6133895e3c88d535ce49dbff62a4633";
          };
      };
      "estraverse-4.2.0" = {
          name = "estraverse";
          packageName = "estraverse";
          version = "4.2.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/estraverse/-/estraverse-4.2.0.tgz";
              sha1 = "0dee3fed31fcd469618ce7342099fc1afa0bdb13";
          };
      };
      "esutils-2.0.2" = {
          name = "esutils";
          packageName = "esutils";
          version = "2.0.2";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/esutils/-/esutils-2.0.2.tgz";
              sha1 = "0abf4f1caa5bcb1f7a9d8acc6dea4faaa04bac9b";
          };
      };
      "expand-template-2.0.3" = {
          name = "expand-template";
          packageName = "expand-template";
          version = "2.0.3";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/expand-template/-/expand-template-2.0.3.tgz";
              sha512 = "XYfuKMvj4O35f/pOXLObndIRvyQ+/+6AhODh+OKWj9S9498pHHn/IMszH+gt0fBCRWMNfk1ZSp5x3AifmnI2vg==";
          };
      };
      "extend-3.0.2" = {
          name = "extend";
          packageName = "extend";
          version = "3.0.2";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/extend/-/extend-3.0.2.tgz";
              sha512 = "fjquC59cD7CyW6urNXK0FBufkZcoiGG80wTuPujX590cB5Ttln20E2UB4S/WARVqhXffZl2LNgS+gQdPIIim/g==";
          };
      };
      "extsprintf-1.3.0" = {
          name = "extsprintf";
          packageName = "extsprintf";
          version = "1.3.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/extsprintf/-/extsprintf-1.3.0.tgz";
              sha1 = "96918440e3041a7a414f8c52e3c574eb3c3e1e05";
          };
      };
      "fast-deep-equal-2.0.1" = {
          name = "fast-deep-equal";
          packageName = "fast-deep-equal";
          version = "2.0.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/fast-deep-equal/-/fast-deep-equal-2.0.1.tgz";
              sha1 = "7b05218ddf9667bf7f370bf7fdb2cb15fdd0aa49";
          };
      };
      "fast-json-stable-stringify-2.0.0" = {
          name = "fast-json-stable-stringify";
          packageName = "fast-json-stable-stringify";
          version = "2.0.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/fast-json-stable-stringify/-/fast-json-stable-stringify-2.0.0.tgz";
              sha1 = "d5142c0caee6b1189f87d3a76111064f86c8bbf2";
          };
      };
      "fast-levenshtein-2.0.6" = {
          name = "fast-levenshtein";
          packageName = "fast-levenshtein";
          version = "2.0.6";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/fast-levenshtein/-/fast-levenshtein-2.0.6.tgz";
              sha1 = "3d8a5c66883a16a30ca8643e851f19baa7797917";
          };
      };
      "forever-agent-0.6.1" = {
          name = "forever-agent";
          packageName = "forever-agent";
          version = "0.6.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/forever-agent/-/forever-agent-0.6.1.tgz";
              sha1 = "fbc71f0c41adeb37f96c577ad1ed42d8fdacca91";
          };
      };
      "form-data-2.3.3" = {
          name = "form-data";
          packageName = "form-data";
          version = "2.3.3";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/form-data/-/form-data-2.3.3.tgz";
              sha512 = "1lLKB2Mu3aGP1Q/2eCOx0fNbRMe7XdwktwOruhfqqd0rIJWwN4Dh+E3hrPSlDCXnSR7UtZ1N38rVXm+6+MEhJQ==";
          };
      };
      "fs-constants-1.0.0" = {
          name = "fs-constants";
          packageName = "fs-constants";
          version = "1.0.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/fs-constants/-/fs-constants-1.0.0.tgz";
              sha512 = "y6OAwoSIf7FyjMIv94u+b5rdheZEjzR63GTyZJm5qh4Bi+2YgwLCcI/fPFZkL5PSixOt6ZNKm+w+Hfp/Bciwow==";
          };
      };
      "fs.realpath-1.0.0" = {
          name = "fs.realpath";
          packageName = "fs.realpath";
          version = "1.0.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/fs.realpath/-/fs.realpath-1.0.0.tgz";
              sha1 = "1504ad2523158caa40db4a2787cb01411994ea4f";
          };
      };
      "gauge-2.7.4" = {
          name = "gauge";
          packageName = "gauge";
          version = "2.7.4";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/gauge/-/gauge-2.7.4.tgz";
              sha1 = "2c03405c7538c39d7eb37b317022e325fb018bf7";
          };
      };
      "getpass-0.1.7" = {
          name = "getpass";
          packageName = "getpass";
          version = "0.1.7";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/getpass/-/getpass-0.1.7.tgz";
              sha1 = "5eff8e3e684d569ae4cb2b1282604e8ba62149fa";
          };
      };
      "github-from-package-0.0.0" = {
          name = "github-from-package";
          packageName = "github-from-package";
          version = "0.0.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/github-from-package/-/github-from-package-0.0.0.tgz";
              sha1 = "97fb5d96bfde8973313f20e8288ef9a167fa64ce";
          };
      };
      "glob-7.1.3" = {
          name = "glob";
          packageName = "glob";
          version = "7.1.3";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/glob/-/glob-7.1.3.tgz";
              sha512 = "vcfuiIxogLV4DlGBHIUOwI0IbrJ8HWPc4MU7HzviGeNho/UJDfi6B5p3sHeWIQ0KGIU0Jpxi5ZHxemQfLkkAwQ==";
          };
      };
      "har-schema-2.0.0" = {
          name = "har-schema";
          packageName = "har-schema";
          version = "2.0.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/har-schema/-/har-schema-2.0.0.tgz";
              sha1 = "a94c2224ebcac04782a0d9035521f24735b7ec92";
          };
      };
      "har-validator-5.1.3" = {
          name = "har-validator";
          packageName = "har-validator";
          version = "5.1.3";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/har-validator/-/har-validator-5.1.3.tgz";
              sha512 = "sNvOCzEQNr/qrvJgc3UG/kD4QtlHycrzwS+6mfTrrSq97BvaYcPZZI1ZSqGSPR73Cxn4LKTD4PttRwfU7jWq5g==";
          };
      };
      "has-unicode-2.0.1" = {
          name = "has-unicode";
          packageName = "has-unicode";
          version = "2.0.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/has-unicode/-/has-unicode-2.0.1.tgz";
              sha1 = "e0e6fe6a28cf51138855e086d1691e771de2a8b9";
          };
      };
      "html-encoding-sniffer-1.0.2" = {
          name = "html-encoding-sniffer";
          packageName = "html-encoding-sniffer";
          version = "1.0.2";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/html-encoding-sniffer/-/html-encoding-sniffer-1.0.2.tgz";
              sha512 = "71lZziiDnsuabfdYiUeWdCVyKuqwWi23L8YeIgV9jSSZHCtb6wB1BKWooH7L3tn4/FuZJMVWyNaIDr4RGmaSYw==";
          };
      };
      "http-signature-1.2.0" = {
          name = "http-signature";
          packageName = "http-signature";
          version = "1.2.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/http-signature/-/http-signature-1.2.0.tgz";
              sha1 = "9aecd925114772f3d95b65a60abb8f7c18fbace1";
          };
      };
      "iconv-lite-0.4.24" = {
          name = "iconv-lite";
          packageName = "iconv-lite";
          version = "0.4.24";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/iconv-lite/-/iconv-lite-0.4.24.tgz";
              sha512 = "v3MXnZAcvnywkTUEZomIActle7RXXeedOR31wwl7VlyoXO4Qi9arvSenNQWne1TcRwhCL1HwLI21bEqdpj8/rA==";
          };
      };
      "inflight-1.0.6" = {
          name = "inflight";
          packageName = "inflight";
          version = "1.0.6";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/inflight/-/inflight-1.0.6.tgz";
              sha1 = "49bd6331d7d02d0c09bc910a1075ba8165b56df9";
          };
      };
      "inherits-2.0.3" = {
          name = "inherits";
          packageName = "inherits";
          version = "2.0.3";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/inherits/-/inherits-2.0.3.tgz";
              sha1 = "633c2c83e3da42a502f52466022480f4208261de";
          };
      };
      "ini-1.3.5" = {
          name = "ini";
          packageName = "ini";
          version = "1.3.5";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/ini/-/ini-1.3.5.tgz";
              sha512 = "RZY5huIKCMRWDUqZlEi72f/lmXKMvuszcMBduliQ3nnWbx9X/ZBQO7DijMEYS9EhHBb2qacRUMtC7svLwe0lcw==";
          };
      };
      "is-fullwidth-code-point-1.0.0" = {
          name = "is-fullwidth-code-point";
          packageName = "is-fullwidth-code-point";
          version = "1.0.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/is-fullwidth-code-point/-/is-fullwidth-code-point-1.0.0.tgz";
              sha1 = "ef9e31386f031a7f0d643af82fde50c457ef00cb";
          };
      };
      "is-typedarray-1.0.0" = {
          name = "is-typedarray";
          packageName = "is-typedarray";
          version = "1.0.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/is-typedarray/-/is-typedarray-1.0.0.tgz";
              sha1 = "e479c80858df0c1b11ddda6940f96011fcda4a9a";
          };
      };
      "isarray-1.0.0" = {
          name = "isarray";
          packageName = "isarray";
          version = "1.0.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/isarray/-/isarray-1.0.0.tgz";
              sha1 = "bb935d48582cba168c06834957a54a3e07124f11";
          };
      };
      "isstream-0.1.2" = {
          name = "isstream";
          packageName = "isstream";
          version = "0.1.2";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/isstream/-/isstream-0.1.2.tgz";
              sha1 = "47e63f7af55afa6f92e1500e690eb8b8529c099a";
          };
      };
      "jsbn-0.1.1" = {
          name = "jsbn";
          packageName = "jsbn";
          version = "0.1.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/jsbn/-/jsbn-0.1.1.tgz";
              sha1 = "a5e654c2e5a2deb5f201d96cefbca80c0ef2f513";
          };
      };
      "jsdom-11.12.0" = {
          name = "jsdom";
          packageName = "jsdom";
          version = "11.12.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/jsdom/-/jsdom-11.12.0.tgz";
              sha512 = "y8Px43oyiBM13Zc1z780FrfNLJCXTL40EWlty/LXUtcjykRBNgLlCjWXpfSPBl2iv+N7koQN+dvqszHZgT/Fjw==";
          };
      };
      "json-schema-0.2.3" = {
          name = "json-schema";
          packageName = "json-schema";
          version = "0.2.3";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/json-schema/-/json-schema-0.2.3.tgz";
              sha1 = "b480c892e59a2f05954ce727bd3f2a4e882f9e13";
          };
      };
      "json-schema-traverse-0.4.1" = {
          name = "json-schema-traverse";
          packageName = "json-schema-traverse";
          version = "0.4.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/json-schema-traverse/-/json-schema-traverse-0.4.1.tgz";
              sha512 = "xbbCH5dCYU5T8LcEhhuh7HJ88HXuW3qsI3Y0zOZFKfZEHcpWiHU/Jxzk629Brsab/mMiHQti9wMP+845RPe3Vg==";
          };
      };
      "json-stringify-safe-5.0.1" = {
          name = "json-stringify-safe";
          packageName = "json-stringify-safe";
          version = "5.0.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/json-stringify-safe/-/json-stringify-safe-5.0.1.tgz";
              sha1 = "1296a2d58fd45f19a0f6ce01d65701e2c735b6eb";
          };
      };
      "jsprim-1.4.1" = {
          name = "jsprim";
          packageName = "jsprim";
          version = "1.4.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/jsprim/-/jsprim-1.4.1.tgz";
              sha1 = "313e66bc1e5cc06e438bc1b7499c2e5c56acb6a2";
          };
      };
      "left-pad-1.3.0" = {
          name = "left-pad";
          packageName = "left-pad";
          version = "1.3.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/left-pad/-/left-pad-1.3.0.tgz";
              sha512 = "XI5MPzVNApjAyhQzphX8BkmKsKUxD4LdyK24iZeQGinBN9yTQT3bFlCBy/aVx2HrNcqQGsdot8ghrjyrvMCoEA==";
          };
      };
      "levn-0.3.0" = {
          name = "levn";
          packageName = "levn";
          version = "0.3.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/levn/-/levn-0.3.0.tgz";
              sha1 = "3b09924edf9f083c0490fdd4c0bc4421e04764ee";
          };
      };
      "lodash-4.17.11" = {
          name = "lodash";
          packageName = "lodash";
          version = "4.17.11";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/lodash/-/lodash-4.17.11.tgz";
              sha512 = "cQKh8igo5QUhZ7lg38DYWAxMvjSAKG0A8wGSVimP07SIUEK2UO+arSRKbRZWtelMtN5V0Hkwh5ryOto/SshYIg==";
          };
      };
      "lodash.sortby-4.7.0" = {
          name = "lodash.sortby";
          packageName = "lodash.sortby";
          version = "4.7.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/lodash.sortby/-/lodash.sortby-4.7.0.tgz";
              sha1 = "edd14c824e2cc9c1e0b0a1b42bb5210516a42438";
          };
      };
      "mime-db-1.40.0" = {
          name = "mime-db";
          packageName = "mime-db";
          version = "1.40.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/mime-db/-/mime-db-1.40.0.tgz";
              sha512 = "jYdeOMPy9vnxEqFRRo6ZvTZ8d9oPb+k18PKoYNYUe2stVEBPPwsln/qWzdbmaIvnhZ9v2P+CuecK+fpUfsV2mA==";
          };
      };
      "mime-types-2.1.24" = {
          name = "mime-types";
          packageName = "mime-types";
          version = "2.1.24";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/mime-types/-/mime-types-2.1.24.tgz";
              sha512 = "WaFHS3MCl5fapm3oLxU4eYDw77IQM2ACcxQ9RIxfaC3ooc6PFuBMGZZsYpvoXS5D5QTWPieo1jjLdAm3TBP3cQ==";
          };
      };
      "mimic-response-1.0.1" = {
          name = "mimic-response";
          packageName = "mimic-response";
          version = "1.0.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/mimic-response/-/mimic-response-1.0.1.tgz";
              sha512 = "j5EctnkH7amfV/q5Hgmoal1g2QHFJRraOtmx0JpIqkxhBhI/lJSl1nMpQ45hVarwNETOoWEimndZ4QK0RHxuxQ==";
          };
      };
      "minimatch-3.0.4" = {
          name = "minimatch";
          packageName = "minimatch";
          version = "3.0.4";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/minimatch/-/minimatch-3.0.4.tgz";
              sha512 = "yJHVQEhyqPLUTgt9B83PXu6W3rx4MvvHvSUvToogpwoGDOUQ+yDrR0HRot+yOCdCO7u4hX3pWft6kWBBcqh0UA==";
          };
      };
      "minimist-1.2.0" = {
          name = "minimist";
          packageName = "minimist";
          version = "1.2.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/minimist/-/minimist-1.2.0.tgz";
              sha1 = "a35008b20f41383eec1fb914f4cd5df79a264284";
          };
      };
      "minimist-0.0.8" = {
          name = "minimist";
          packageName = "minimist";
          version = "0.0.8";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/minimist/-/minimist-0.0.8.tgz";
              sha1 = "857fcabfc3397d2625b8228262e86aa7a011b05d";
          };
      };
      "mkdirp-0.5.1" = {
          name = "mkdirp";
          packageName = "mkdirp";
          version = "0.5.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/mkdirp/-/mkdirp-0.5.1.tgz";
              sha1 = "30057438eac6cf7f8c4767f38648d6697d75c903";
          };
      };
      "nan-2.13.2" = {
          name = "nan";
          packageName = "nan";
          version = "2.13.2";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/nan/-/nan-2.13.2.tgz";
              sha512 = "TghvYc72wlMGMVMluVo9WRJc0mB8KxxF/gZ4YYFy7V2ZQX9l7rgbPg7vjS9mt6U5HXODVFVI2bOduCzwOMv/lw==";
          };
      };
      "napi-build-utils-1.0.1" = {
          name = "napi-build-utils";
          packageName = "napi-build-utils";
          version = "1.0.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/napi-build-utils/-/napi-build-utils-1.0.1.tgz";
              sha512 = "boQj1WFgQH3v4clhu3mTNfP+vOBxorDlE8EKiMjUlLG3C4qAESnn9AxIOkFgTR2c9LtzNjPrjS60cT27ZKBhaA==";
          };
      };
      "node-abi-2.8.0" = {
          name = "node-abi";
          packageName = "node-abi";
          version = "2.8.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/node-abi/-/node-abi-2.8.0.tgz";
              sha512 = "1/aa2clS0pue0HjckL62CsbhWWU35HARvBDXcJtYKbYR7LnIutmpxmXbuDMV9kEviD2lP/wACOgWmmwljghHyQ==";
          };
      };
      "noop-logger-0.1.1" = {
          name = "noop-logger";
          packageName = "noop-logger";
          version = "0.1.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/noop-logger/-/noop-logger-0.1.1.tgz";
              sha1 = "94a2b1633c4f1317553007d8966fd0e841b6a4c2";
          };
      };
      "npmlog-4.1.2" = {
          name = "npmlog";
          packageName = "npmlog";
          version = "4.1.2";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/npmlog/-/npmlog-4.1.2.tgz";
              sha512 = "2uUqazuKlTaSI/dC8AzicUck7+IrEaOnN/e0jd3Xtt1KcGpwx30v50mL7oPyr/h9bL3E4aZccVwpwP+5W9Vjkg==";
          };
      };
      "number-is-nan-1.0.1" = {
          name = "number-is-nan";
          packageName = "number-is-nan";
          version = "1.0.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/number-is-nan/-/number-is-nan-1.0.1.tgz";
              sha1 = "097b602b53422a522c1afb8790318336941a011d";
          };
      };
      "nwsapi-2.1.4" = {
          name = "nwsapi";
          packageName = "nwsapi";
          version = "2.1.4";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/nwsapi/-/nwsapi-2.1.4.tgz";
              sha512 = "iGfd9Y6SFdTNldEy2L0GUhcarIutFmk+MPWIn9dmj8NMIup03G08uUF2KGbbmv/Ux4RT0VZJoP/sVbWA6d/VIw==";
          };
      };
      "oauth-sign-0.9.0" = {
          name = "oauth-sign";
          packageName = "oauth-sign";
          version = "0.9.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/oauth-sign/-/oauth-sign-0.9.0.tgz";
              sha512 = "fexhUFFPTGV8ybAtSIGbV6gOkSv8UtRbDBnAyLQw4QPKkgNlsH2ByPGtMUqdWkos6YCRmAqViwgZrJc/mRDzZQ==";
          };
      };
      "object-assign-4.1.1" = {
          name = "object-assign";
          packageName = "object-assign";
          version = "4.1.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/object-assign/-/object-assign-4.1.1.tgz";
              sha1 = "2109adc7965887cfc05cbbd442cac8bfbb360863";
          };
      };
      "once-1.4.0" = {
          name = "once";
          packageName = "once";
          version = "1.4.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/once/-/once-1.4.0.tgz";
              sha1 = "583b1aa775961d4b113ac17d9c50baef9dd76bd1";
          };
      };
      "optionator-0.8.2" = {
          name = "optionator";
          packageName = "optionator";
          version = "0.8.2";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/optionator/-/optionator-0.8.2.tgz";
              sha1 = "364c5e409d3f4d6301d6c0b4c05bba50180aeb64";
          };
      };
      "os-homedir-1.0.2" = {
          name = "os-homedir";
          packageName = "os-homedir";
          version = "1.0.2";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/os-homedir/-/os-homedir-1.0.2.tgz";
              sha1 = "ffbc4988336e0e833de0c168c7ef152121aa7fb3";
          };
      };
      "parse5-4.0.0" = {
          name = "parse5";
          packageName = "parse5";
          version = "4.0.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/parse5/-/parse5-4.0.0.tgz";
              sha512 = "VrZ7eOd3T1Fk4XWNXMgiGBK/z0MG48BWG2uQNU4I72fkQuKUTZpl+u9k+CxEG0twMVzSmXEEz12z5Fnw1jIQFA==";
          };
      };
      "path-is-absolute-1.0.1" = {
          name = "path-is-absolute";
          packageName = "path-is-absolute";
          version = "1.0.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/path-is-absolute/-/path-is-absolute-1.0.1.tgz";
              sha1 = "174b9268735534ffbc7ace6bf53a5a9e1b5c5f5f";
          };
      };
      "performance-now-2.1.0" = {
          name = "performance-now";
          packageName = "performance-now";
          version = "2.1.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/performance-now/-/performance-now-2.1.0.tgz";
              sha1 = "6309f4e0e5fa913ec1c69307ae364b4b377c9e7b";
          };
      };
      "pn-1.1.0" = {
          name = "pn";
          packageName = "pn";
          version = "1.1.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/pn/-/pn-1.1.0.tgz";
              sha512 = "2qHaIQr2VLRFoxe2nASzsV6ef4yOOH+Fi9FBOVH6cqeSgUnoyySPZkxzLuzd+RYOQTRpROA0ztTMqxROKSb/nA==";
          };
      };
      "prebuild-install-5.3.0" = {
          name = "prebuild-install";
          packageName = "prebuild-install";
          version = "5.3.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/prebuild-install/-/prebuild-install-5.3.0.tgz";
              sha512 = "aaLVANlj4HgZweKttFNUVNRxDukytuIuxeK2boIMHjagNJCiVKWFsKF4tCE3ql3GbrD2tExPQ7/pwtEJcHNZeg==";
          };
      };
      "prelude-ls-1.1.2" = {
          name = "prelude-ls";
          packageName = "prelude-ls";
          version = "1.1.2";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/prelude-ls/-/prelude-ls-1.1.2.tgz";
              sha1 = "21932a549f5e52ffd9a827f570e04be62a97da54";
          };
      };
      "process-nextick-args-2.0.0" = {
          name = "process-nextick-args";
          packageName = "process-nextick-args";
          version = "2.0.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/process-nextick-args/-/process-nextick-args-2.0.0.tgz";
              sha512 = "MtEC1TqN0EU5nephaJ4rAtThHtC86dNN9qCuEhtshvpVBkAW5ZO7BASN9REnF9eoXGcRub+pFuKEpOHE+HbEMw==";
          };
      };
      "psl-1.1.31" = {
          name = "psl";
          packageName = "psl";
          version = "1.1.31";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/psl/-/psl-1.1.31.tgz";
              sha512 = "/6pt4+C+T+wZUieKR620OpzN/LlnNKuWjy1iFLQ/UG35JqHlR/89MP1d96dUfkf6Dne3TuLQzOYEYshJ+Hx8mw==";
          };
      };
      "pump-2.0.1" = {
          name = "pump";
          packageName = "pump";
          version = "2.0.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/pump/-/pump-2.0.1.tgz";
              sha512 = "ruPMNRkN3MHP1cWJc9OWr+T/xDP0jhXYCLfJcBuX54hhfIBnaQmAUMfDcG4DM5UMWByBbJY69QSphm3jtDKIkA==";
          };
      };
      "punycode-2.1.1" = {
          name = "punycode";
          packageName = "punycode";
          version = "2.1.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/punycode/-/punycode-2.1.1.tgz";
              sha512 = "XRsRjdf+j5ml+y/6GKHPZbrF/8p2Yga0JPtdqTIY2Xe5ohJPD9saDJJLPvp9+NSBprVvevdXZybnj2cv8OEd0A==";
          };
      };
      "qs-6.5.2" = {
          name = "qs";
          packageName = "qs";
          version = "6.5.2";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/qs/-/qs-6.5.2.tgz";
              sha512 = "N5ZAX4/LxJmF+7wN74pUD6qAh9/wnvdQcjq9TZjevvXzSUo7bfmw91saqMjzGS2xq91/odN2dW/WOl7qQHNDGA==";
          };
      };
      "rc-1.2.8" = {
          name = "rc";
          packageName = "rc";
          version = "1.2.8";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/rc/-/rc-1.2.8.tgz";
              sha512 = "y3bGgqKj3QBdxLbLkomlohkvsA8gdAiUQlSBJnBhfn+BPxg4bc62d8TcBW15wavDfgexCgccckhcZvywyQYPOw==";
          };
      };
      "readable-stream-2.3.6" = {
          name = "readable-stream";
          packageName = "readable-stream";
          version = "2.3.6";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/readable-stream/-/readable-stream-2.3.6.tgz";
              sha512 = "tQtKA9WIAhBF3+VLAseyMqZeBjW0AHJoxOtYqSUZNJxauErmLbVm2FW1y+J/YA9dUrAC39ITejlZWhVIwawkKw==";
          };
      };
      "request-2.88.0" = {
          name = "request";
          packageName = "request";
          version = "2.88.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/request/-/request-2.88.0.tgz";
              sha512 = "NAqBSrijGLZdM0WZNsInLJpkJokL72XYjUpnB0iwsRgxh7dB6COrHnTBNwN0E+lHDAJzu7kLAkDeY08z2/A0hg==";
          };
      };
      "request-promise-core-1.1.2" = {
          name = "request-promise-core";
          packageName = "request-promise-core";
          version = "1.1.2";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/request-promise-core/-/request-promise-core-1.1.2.tgz";
              sha512 = "UHYyq1MO8GsefGEt7EprS8UrXsm1TxEvFUX1IMTuSLU2Rh7fTIdFtl8xD7JiEYiWU2dl+NYAjCTksTehQUxPag==";
          };
      };
      "request-promise-native-1.0.7" = {
          name = "request-promise-native";
          packageName = "request-promise-native";
          version = "1.0.7";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/request-promise-native/-/request-promise-native-1.0.7.tgz";
              sha512 = "rIMnbBdgNViL37nZ1b3L/VfPOpSi0TqVDQPAvO6U14lMzOLrt5nilxCQqtDKhZeDiW0/hkCXGoQjhgJd/tCh6w==";
          };
      };
      "safe-buffer-5.1.2" = {
          name = "safe-buffer";
          packageName = "safe-buffer";
          version = "5.1.2";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/safe-buffer/-/safe-buffer-5.1.2.tgz";
              sha512 = "Gd2UZBJDkXlY7GbJxfsE8/nvKkUEU1G38c1siN6QP6a9PT9MmHB8GnpscSmMJSoF8LOIrt8ud/wPtojys4G6+g==";
          };
      };
      "safer-buffer-2.1.2" = {
          name = "safer-buffer";
          packageName = "safer-buffer";
          version = "2.1.2";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/safer-buffer/-/safer-buffer-2.1.2.tgz";
              sha512 = "YZo3K82SD7Riyi0E1EQPojLz7kpepnSQI9IyPbHHg1XXXevb5dJI7tpyN2ADxGcQbHG7vcyRHk0cbwqcQriUtg==";
          };
      };
      "sax-1.2.4" = {
          name = "sax";
          packageName = "sax";
          version = "1.2.4";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/sax/-/sax-1.2.4.tgz";
              sha512 = "NqVDv9TpANUjFm0N8uM5GxL36UgKi9/atZw+x7YFnQ8ckwFGKrl4xX4yWtrey3UJm5nP1kUbnYgLopqWNSRhWw==";
          };
      };
      "semver-5.7.0" = {
          name = "semver";
          packageName = "semver";
          version = "5.7.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/semver/-/semver-5.7.0.tgz";
              sha512 = "Ya52jSX2u7QKghxeoFGpLwCtGlt7j0oY9DYb5apt9nPlJ42ID+ulTXESnt/qAQcoSERyZ5sl3LDIOw0nAn/5DA==";
          };
      };
      "set-blocking-2.0.0" = {
          name = "set-blocking";
          packageName = "set-blocking";
          version = "2.0.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/set-blocking/-/set-blocking-2.0.0.tgz";
              sha1 = "045f9782d011ae9a6803ddd382b24392b3d890f7";
          };
      };
      "signal-exit-3.0.2" = {
          name = "signal-exit";
          packageName = "signal-exit";
          version = "3.0.2";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/signal-exit/-/signal-exit-3.0.2.tgz";
              sha1 = "b5fdc08f1287ea1178628e415e25132b73646c6d";
          };
      };
      "simple-concat-1.0.0" = {
          name = "simple-concat";
          packageName = "simple-concat";
          version = "1.0.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/simple-concat/-/simple-concat-1.0.0.tgz";
              sha1 = "7344cbb8b6e26fb27d66b2fc86f9f6d5997521c6";
          };
      };
      "simple-get-2.8.1" = {
          name = "simple-get";
          packageName = "simple-get";
          version = "2.8.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/simple-get/-/simple-get-2.8.1.tgz";
              sha512 = "lSSHRSw3mQNUGPAYRqo7xy9dhKmxFXIjLjp4KHpf99GEH2VH7C3AM+Qfx6du6jhfUi6Vm7XnbEVEf7Wb6N8jRw==";
          };
      };
      "source-map-0.6.1" = {
          name = "source-map";
          packageName = "source-map";
          version = "0.6.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/source-map/-/source-map-0.6.1.tgz";
              sha512 = "UjgapumWlbMhkBgzT7Ykc5YXUT46F0iKu8SGXq0bcwP5dz/h0Plj6enJqjz1Zbq2l5WaqYnrVbwWOWMyF3F47g==";
          };
      };
      "sshpk-1.16.1" = {
          name = "sshpk";
          packageName = "sshpk";
          version = "1.16.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/sshpk/-/sshpk-1.16.1.tgz";
              sha512 = "HXXqVUq7+pcKeLqqZj6mHFUMvXtOJt1uoUx09pFW6011inTMxqI8BA8PM95myrIyyKwdnzjdFjLiE6KBPVtJIg==";
          };
      };
      "stealthy-require-1.1.1" = {
          name = "stealthy-require";
          packageName = "stealthy-require";
          version = "1.1.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/stealthy-require/-/stealthy-require-1.1.1.tgz";
              sha1 = "35b09875b4ff49f26a777e509b3090a3226bf24b";
          };
      };
      "string-width-1.0.2" = {
          name = "string-width";
          packageName = "string-width";
          version = "1.0.2";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/string-width/-/string-width-1.0.2.tgz";
              sha1 = "118bdf5b8cdc51a2a7e70d211e07e2b0b9b107d3";
          };
      };
      "string_decoder-1.1.1" = {
          name = "string_decoder";
          packageName = "string_decoder";
          version = "1.1.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/string_decoder/-/string_decoder-1.1.1.tgz";
              sha512 = "n/ShnvDi6FHbbVfviro+WojiFzv+s8MPMHBczVePfUpDJLwoLT0ht1l4YwBCbi8pJAveEEdnkHyPyTP/mzRfwg==";
          };
      };
      "strip-ansi-3.0.1" = {
          name = "strip-ansi";
          packageName = "strip-ansi";
          version = "3.0.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/strip-ansi/-/strip-ansi-3.0.1.tgz";
              sha1 = "6a385fb8853d952d5ff05d0e8aaf94278dc63dcf";
          };
      };
      "strip-json-comments-2.0.1" = {
          name = "strip-json-comments";
          packageName = "strip-json-comments";
          version = "2.0.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/strip-json-comments/-/strip-json-comments-2.0.1.tgz";
              sha1 = "3c531942e908c2697c0ec344858c286c7ca0a60a";
          };
      };
      "symbol-tree-3.2.2" = {
          name = "symbol-tree";
          packageName = "symbol-tree";
          version = "3.2.2";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/symbol-tree/-/symbol-tree-3.2.2.tgz";
              sha1 = "ae27db38f660a7ae2e1c3b7d1bc290819b8519e6";
          };
      };
      "pump-1.0.3" = {
          name = "pump";
          packageName = "pump";
          version = "1.0.3";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/pump/-/pump-1.0.3.tgz";
              sha512 = "8k0JupWme55+9tCVE+FS5ULT3K6AbgqrGa58lTT49RpyfwwcGedHqaC5LlQNdEAumn/wFsu6aPwkuPMioy8kqw==";
          };
      };
      "tar-fs-1.16.3" = {
          name = "tar-fs";
          packageName = "tar-fs";
          version = "1.16.3";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/tar-fs/-/tar-fs-1.16.3.tgz";
              sha512 = "NvCeXpYx7OsmOh8zIOP/ebG55zZmxLE0etfWRbWok+q2Qo8x/vOR/IJT1taADXPe+jsiu9axDb3X4B+iIgNlKw==";
          };
      };
      "tar-stream-1.6.2" = {
          name = "tar-stream";
          packageName = "tar-stream";
          version = "1.6.2";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/tar-stream/-/tar-stream-1.6.2.tgz";
              sha512 = "rzS0heiNf8Xn7/mpdSVVSMAWAoy9bfb1WOTYC78Z0UQKeKa/CWS8FOq0lKGNa8DWKAn9gxjCvMLYc5PGXYlK2A==";
          };
      };
      "to-buffer-1.1.1" = {
          name = "to-buffer";
          packageName = "to-buffer";
          version = "1.1.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/to-buffer/-/to-buffer-1.1.1.tgz";
              sha512 = "lx9B5iv7msuFYE3dytT+KE5tap+rNYw+K4jVkb9R/asAb+pbBSM17jtunHplhBe6RRJdZx3Pn2Jph24O32mOVg==";
          };
      };
      "punycode-1.4.1" = {
          name = "punycode";
          packageName = "punycode";
          version = "1.4.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/punycode/-/punycode-1.4.1.tgz";
              sha1 = "c0d5a63b2718800ad8e1eb0fa5269c84dd41845e";
          };
      };
      "tough-cookie-2.4.3" = {
          name = "tough-cookie";
          packageName = "tough-cookie";
          version = "2.4.3";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/tough-cookie/-/tough-cookie-2.4.3.tgz";
              sha512 = "Q5srk/4vDM54WJsJio3XNn6K2sCG+CQ8G5Wz6bZhRZoAe/+TxjWB/GlFAnYEbkYVlON9FMk/fE3h2RLpPXo4lQ==";
          };
      };
      "tr46-1.0.1" = {
          name = "tr46";
          packageName = "tr46";
          version = "1.0.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/tr46/-/tr46-1.0.1.tgz";
              sha1 = "a8b13fd6bfd2489519674ccde55ba3693b706d09";
          };
      };
      "tree-sitter-0.13.23" = {
          name = "tree-sitter";
          packageName = "tree-sitter";
          version = "0.13.23";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/tree-sitter/-/tree-sitter-0.13.23.tgz";
              sha512 = "75AiPbMEstv+YK8h4FkAHnmAJ6nNIUj/NFzRvKCHovmwSEKMi8Wc/E/crB4lJnHBOfV/f/DMQjN+e1Y36kagug==";
          };
      };
      "tree-sitter-bash-0.13.9" = {
          name = "tree-sitter-bash";
          packageName = "tree-sitter-bash";
          version = "0.13.9";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/tree-sitter-bash/-/tree-sitter-bash-0.13.9.tgz";
              sha512 = "b0L+QLS2eeIVrHnnbkFlvO1nElhPwqTxLIwyTeJytPYT0TS50Pe7bP+uPi3gkHT1YajxcauCxX1aDWDiZK1h5Q==";
          };
      };
      "tunnel-agent-0.6.0" = {
          name = "tunnel-agent";
          packageName = "tunnel-agent";
          version = "0.6.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/tunnel-agent/-/tunnel-agent-0.6.0.tgz";
              sha1 = "27a5dea06b36b04a0a9966774b290868f0fc40fd";
          };
      };
      "turndown-4.0.2" = {
          name = "turndown";
          packageName = "turndown";
          version = "4.0.2";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/turndown/-/turndown-4.0.2.tgz";
              sha512 = "pqZ6WrHFGnxXC9q2xJ3Qa7EoLAwrojgFRajWZjxTKwbz9vnNnyi8lLjiD5h86UTPOcMlEyHjm6NMhjEDdlc25A==";
          };
      };
      "tweetnacl-0.14.5" = {
          name = "tweetnacl";
          packageName = "tweetnacl";
          version = "0.14.5";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/tweetnacl/-/tweetnacl-0.14.5.tgz";
              sha1 = "5ae68177f192d4456269d108afa93ff8743f4f64";
          };
      };
      "type-check-0.3.2" = {
          name = "type-check";
          packageName = "type-check";
          version = "0.3.2";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/type-check/-/type-check-0.3.2.tgz";
              sha1 = "5884cab512cf1d355e3fb784f30804b2b520db72";
          };
      };
      "uri-js-4.2.2" = {
          name = "uri-js";
          packageName = "uri-js";
          version = "4.2.2";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/uri-js/-/uri-js-4.2.2.tgz";
              sha512 = "KY9Frmirql91X2Qgjry0Wd4Y+YTdrdZheS8TFwvkbLWf/G5KNJDCh6pKL5OZctEW4+0Baa5idK2ZQuELRwPznQ==";
          };
      };
      "urijs-1.19.1" = {
          name = "urijs";
          packageName = "urijs";
          version = "1.19.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/urijs/-/urijs-1.19.1.tgz";
              sha512 = "xVrGVi94ueCJNrBSTjWqjvtgvl3cyOTThp2zaMaFNGp3F542TR6sM3f2o8RqZl+AwteClSVmoCyt0ka4RjQOQg==";
          };
      };
      "util-deprecate-1.0.2" = {
          name = "util-deprecate";
          packageName = "util-deprecate";
          version = "1.0.2";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/util-deprecate/-/util-deprecate-1.0.2.tgz";
              sha1 = "450d4dc9fa70de732762fbd2d4a28981419a0ccf";
          };
      };
      "uuid-3.3.2" = {
          name = "uuid";
          packageName = "uuid";
          version = "3.3.2";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/uuid/-/uuid-3.3.2.tgz";
              sha512 = "yXJmeNaw3DnnKAOKJE51sL/ZaYfWJRl1pK9dr19YFCu0ObS231AB1/LbqTKRAQ5kw8A90rA6fr4riOUpTZvQZA==";
          };
      };
      "verror-1.10.0" = {
          name = "verror";
          packageName = "verror";
          version = "1.10.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/verror/-/verror-1.10.0.tgz";
              sha1 = "3a105ca17053af55d6e270c1f8288682e18da400";
          };
      };
      "vscode-jsonrpc-4.0.0" = {
          name = "vscode-jsonrpc";
          packageName = "vscode-jsonrpc";
          version = "4.0.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/vscode-jsonrpc/-/vscode-jsonrpc-4.0.0.tgz";
              sha512 = "perEnXQdQOJMTDFNv+UF3h1Y0z4iSiaN9jIlb0OqIYgosPCZGYh/MCUlkFtV2668PL69lRDO32hmvL2yiidUYg==";
          };
      };
      "vscode-languageserver-4.4.2" = {
          name = "vscode-languageserver";
          packageName = "vscode-languageserver";
          version = "4.4.2";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/vscode-languageserver/-/vscode-languageserver-4.4.2.tgz";
              sha512 = "61y8Raevi9EigDgg9NelvT9cUAohiEbUl1LOwQQgOCAaNX62yKny/ddi0uC+FUTm4CzsjhBu+06R+vYgfCYReA==";
          };
      };
      "vscode-languageserver-protocol-3.14.1" = {
          name = "vscode-languageserver-protocol";
          packageName = "vscode-languageserver-protocol";
          version = "3.14.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/vscode-languageserver-protocol/-/vscode-languageserver-protocol-3.14.1.tgz";
              sha512 = "IL66BLb2g20uIKog5Y2dQ0IiigW0XKrvmWiOvc0yXw80z3tMEzEnHjaGAb3ENuU7MnQqgnYJ1Cl2l9RvNgDi4g==";
          };
      };
      "vscode-languageserver-types-3.14.0" = {
          name = "vscode-languageserver-types";
          packageName = "vscode-languageserver-types";
          version = "3.14.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/vscode-languageserver-types/-/vscode-languageserver-types-3.14.0.tgz";
              sha512 = "lTmS6AlAlMHOvPQemVwo3CezxBp0sNB95KNPkqp3Nxd5VFEnuG1ByM0zlRWos0zjO3ZWtkvhal0COgiV1xIA4A==";
          };
      };
      "vscode-uri-1.0.6" = {
          name = "vscode-uri";
          packageName = "vscode-uri";
          version = "1.0.6";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/vscode-uri/-/vscode-uri-1.0.6.tgz";
              sha512 = "sLI2L0uGov3wKVb9EB+vIQBl9tVP90nqRvxSoJ35vI3NjxE8jfsE5DSOhWgSunHSZmKS4OCi2jrtfxK7uyp2ww==";
          };
      };
      "w3c-hr-time-1.0.1" = {
          name = "w3c-hr-time";
          packageName = "w3c-hr-time";
          version = "1.0.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/w3c-hr-time/-/w3c-hr-time-1.0.1.tgz";
              sha1 = "82ac2bff63d950ea9e3189a58a65625fedf19045";
          };
      };
      "webidl-conversions-4.0.2" = {
          name = "webidl-conversions";
          packageName = "webidl-conversions";
          version = "4.0.2";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/webidl-conversions/-/webidl-conversions-4.0.2.tgz";
              sha512 = "YQ+BmxuTgd6UXZW3+ICGfyqRyHXVlD5GtQr5+qjiNW7bF0cqrzX500HVXPBOvgXb5YnzDd+h0zqyv61KUD7+Sg==";
          };
      };
      "whatwg-encoding-1.0.5" = {
          name = "whatwg-encoding";
          packageName = "whatwg-encoding";
          version = "1.0.5";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/whatwg-encoding/-/whatwg-encoding-1.0.5.tgz";
              sha512 = "b5lim54JOPN9HtzvK9HFXvBma/rnfFeqsic0hSpjtDbVxR3dJKLc+KB4V6GgiGOvl7CY/KNh8rxSo9DKQrnUEw==";
          };
      };
      "whatwg-mimetype-2.3.0" = {
          name = "whatwg-mimetype";
          packageName = "whatwg-mimetype";
          version = "2.3.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/whatwg-mimetype/-/whatwg-mimetype-2.3.0.tgz";
              sha512 = "M4yMwr6mAnQz76TbJm914+gPpB/nCwvZbJU28cUD6dR004SAxDLOOSUaB1JDRqLtaOV/vi0IC5lEAGFgrjGv/g==";
          };
      };
      "whatwg-url-6.5.0" = {
          name = "whatwg-url";
          packageName = "whatwg-url";
          version = "6.5.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/whatwg-url/-/whatwg-url-6.5.0.tgz";
              sha512 = "rhRZRqx/TLJQWUpQ6bmrt2UV4f0HCQ463yQuONJqC6fO2VoEb1pTYddbe59SkYq87aoM5A3bdhMZiUiVws+fzQ==";
          };
      };
      "which-pm-runs-1.0.0" = {
          name = "which-pm-runs";
          packageName = "which-pm-runs";
          version = "1.0.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/which-pm-runs/-/which-pm-runs-1.0.0.tgz";
              sha1 = "670b3afbc552e0b55df6b7780ca74615f23ad1cb";
          };
      };
      "wide-align-1.1.3" = {
          name = "wide-align";
          packageName = "wide-align";
          version = "1.1.3";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/wide-align/-/wide-align-1.1.3.tgz";
              sha512 = "QGkOQc8XL6Bt5PwnsExKBPuMKBxnGxWWW3fU55Xt4feHozMUhdUMaBCk290qpm/wG5u/RSKzwdAC4i51YigihA==";
          };
      };
      "wordwrap-1.0.0" = {
          name = "wordwrap";
          packageName = "wordwrap";
          version = "1.0.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/wordwrap/-/wordwrap-1.0.0.tgz";
              sha1 = "27584810891456a4171c8d0226441ade90cbcaeb";
          };
      };
      "wrappy-1.0.2" = {
          name = "wrappy";
          packageName = "wrappy";
          version = "1.0.2";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/wrappy/-/wrappy-1.0.2.tgz";
              sha1 = "b5243d8f3ec1aa35f1364605bc0d1036e30ab69f";
          };
      };
      "ws-5.2.2" = {
          name = "ws";
          packageName = "ws";
          version = "5.2.2";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/ws/-/ws-5.2.2.tgz";
              sha512 = "jaHFD6PFv6UgoIVda6qZllptQsMlDEJkTQcybzzXDYM1XO9Y8em691FGMPmM46WGyLU4z9KMgQN+qrux/nhlHA==";
          };
      };
      "xml-name-validator-3.0.0" = {
          name = "xml-name-validator";
          packageName = "xml-name-validator";
          version = "3.0.0";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/xml-name-validator/-/xml-name-validator-3.0.0.tgz";
              sha512 = "A5CUptxDsvxKJEU3yO6DuWBSJz/qizqzJKOMIfUJHETbBw/sFaDxgd6fxm1ewUaM0jZ444Fc5vC5ROYurg/4Pw==";
          };
      };
      "xtend-4.0.1" = {
          name = "xtend";
          packageName = "xtend";
          version = "4.0.1";
          src = super.fetchurl {
              url = "https://registry.npmjs.org/xtend/-/xtend-4.0.1.tgz";
              sha1 = "a5c6d532be656e23db820efb943a1f04998d63af";
          };
      };
    };
in rec {

    bash-language-server = nodeEnv.buildNodePackage {
        name = "bash-language-server";
        packageName = "bash-language-server";
        version = "1.5.6";
        src = super.fetchurl {
            url = "https://registry.npmjs.org/bash-language-server/-/bash-language-server-1.5.6.tgz";
            sha512 = "GqW24myNihrUroXdL40AemizC3lBvr2wOiF0GzxYWqsBsJvi6CQor0Y9t10jOGn11evMhncmR2f2LoQdjsNpqw==";
        };
        dependencies = [
            sources."abab-2.0.0"
            sources."acorn-5.7.3"
            (sources."acorn-globals-4.3.2" // {
                dependencies = [
                    sources."acorn-6.1.1"
                ];
            })
            sources."acorn-walk-6.1.1"
            sources."ajv-6.10.0"
            sources."ansi-regex-2.1.1"
            sources."aproba-1.2.0"
            sources."are-we-there-yet-1.1.5"
            sources."array-equal-1.0.0"
            sources."asn1-0.2.4"
            sources."assert-plus-1.0.0"
            sources."async-limiter-1.0.0"
            sources."asynckit-0.4.0"
            sources."aws-sign2-0.7.0"
            sources."aws4-1.8.0"
            sources."balanced-match-1.0.0"
            sources."bcrypt-pbkdf-1.0.2"
            sources."bl-1.2.2"
            sources."brace-expansion-1.1.11"
            sources."browser-process-hrtime-0.1.3"
            sources."buffer-alloc-1.2.0"
            sources."buffer-alloc-unsafe-1.1.0"
            sources."buffer-fill-1.0.0"
            sources."caseless-0.12.0"
            sources."chownr-1.1.1"
            sources."code-point-at-1.1.0"
            sources."combined-stream-1.0.7"
            sources."concat-map-0.0.1"
            sources."console-control-strings-1.1.0"
            sources."core-util-is-1.0.2"
            sources."cssom-0.3.6"
            sources."cssstyle-1.2.2"
            sources."dashdash-1.14.1"
            (sources."data-urls-1.1.0" // {
                dependencies = [
                    sources."whatwg-url-7.0.0"
                ];
            })
            sources."decompress-response-3.3.0"
            sources."deep-extend-0.6.0"
            sources."deep-is-0.1.3"
            sources."delayed-stream-1.0.0"
            sources."delegates-1.0.0"
            sources."detect-libc-1.0.3"
            sources."domexception-1.0.1"
            sources."ecc-jsbn-0.1.2"
            sources."end-of-stream-1.4.1"
            sources."escodegen-1.11.1"
            sources."esprima-3.1.3"
            sources."estraverse-4.2.0"
            sources."esutils-2.0.2"
            sources."expand-template-2.0.3"
            sources."extend-3.0.2"
            sources."extsprintf-1.3.0"
            sources."fast-deep-equal-2.0.1"
            sources."fast-json-stable-stringify-2.0.0"
            sources."fast-levenshtein-2.0.6"
            sources."forever-agent-0.6.1"
            sources."form-data-2.3.3"
            sources."fs-constants-1.0.0"
            sources."fs.realpath-1.0.0"
            sources."gauge-2.7.4"
            sources."getpass-0.1.7"
            sources."github-from-package-0.0.0"
            sources."glob-7.1.3"
            sources."har-schema-2.0.0"
            sources."har-validator-5.1.3"
            sources."has-unicode-2.0.1"
            sources."html-encoding-sniffer-1.0.2"
            sources."http-signature-1.2.0"
            sources."iconv-lite-0.4.24"
            sources."inflight-1.0.6"
            sources."inherits-2.0.3"
            sources."ini-1.3.5"
            sources."is-fullwidth-code-point-1.0.0"
            sources."is-typedarray-1.0.0"
            sources."isarray-1.0.0"
            sources."isstream-0.1.2"
            sources."jsbn-0.1.1"
            sources."jsdom-11.12.0"
            sources."json-schema-0.2.3"
            sources."json-schema-traverse-0.4.1"
            sources."json-stringify-safe-5.0.1"
            sources."jsprim-1.4.1"
            sources."left-pad-1.3.0"
            sources."levn-0.3.0"
            sources."lodash-4.17.11"
            sources."lodash.sortby-4.7.0"
            sources."mime-db-1.40.0"
            sources."mime-types-2.1.24"
            sources."mimic-response-1.0.1"
            sources."minimatch-3.0.4"
            sources."minimist-1.2.0"
            (sources."mkdirp-0.5.1" // {
                dependencies = [
                    sources."minimist-0.0.8"
                ];
            })
            sources."nan-2.13.2"
            sources."napi-build-utils-1.0.1"
            sources."node-abi-2.8.0"
            sources."noop-logger-0.1.1"
            sources."npmlog-4.1.2"
            sources."number-is-nan-1.0.1"
            sources."nwsapi-2.1.4"
            sources."oauth-sign-0.9.0"
            sources."object-assign-4.1.1"
            sources."once-1.4.0"
            sources."optionator-0.8.2"
            sources."os-homedir-1.0.2"
            sources."parse5-4.0.0"
            sources."path-is-absolute-1.0.1"
            sources."performance-now-2.1.0"
            sources."pn-1.1.0"
            sources."prebuild-install-5.3.0"
            sources."prelude-ls-1.1.2"
            sources."process-nextick-args-2.0.0"
            sources."psl-1.1.31"
            sources."pump-2.0.1"
            sources."punycode-2.1.1"
            sources."qs-6.5.2"
            sources."rc-1.2.8"
            sources."readable-stream-2.3.6"
            sources."request-2.88.0"
            sources."request-promise-core-1.1.2"
            sources."request-promise-native-1.0.7"
            sources."safe-buffer-5.1.2"
            sources."safer-buffer-2.1.2"
            sources."sax-1.2.4"
            sources."semver-5.7.0"
            sources."set-blocking-2.0.0"
            sources."signal-exit-3.0.2"
            sources."simple-concat-1.0.0"
            sources."simple-get-2.8.1"
            sources."source-map-0.6.1"
            sources."sshpk-1.16.1"
            sources."stealthy-require-1.1.1"
            sources."string-width-1.0.2"
            sources."string_decoder-1.1.1"
            sources."strip-ansi-3.0.1"
            sources."strip-json-comments-2.0.1"
            sources."symbol-tree-3.2.2"
            (sources."tar-fs-1.16.3" // {
                dependencies = [
                    sources."pump-1.0.3"
                ];
            })
            sources."tar-stream-1.6.2"
            sources."to-buffer-1.1.1"
            (sources."tough-cookie-2.4.3" // {
                dependencies = [
                    sources."punycode-1.4.1"
                ];
            })
            sources."tr46-1.0.1"
            sources."tree-sitter-0.13.23"
            sources."tree-sitter-bash-0.13.9"
            sources."tunnel-agent-0.6.0"
            sources."turndown-4.0.2"
            sources."tweetnacl-0.14.5"
            sources."type-check-0.3.2"
            sources."uri-js-4.2.2"
            sources."urijs-1.19.1"
            sources."util-deprecate-1.0.2"
            sources."uuid-3.3.2"
            sources."verror-1.10.0"
            sources."vscode-jsonrpc-4.0.0"
            sources."vscode-languageserver-4.4.2"
            sources."vscode-languageserver-protocol-3.14.1"
            sources."vscode-languageserver-types-3.14.0"
            sources."vscode-uri-1.0.6"
            sources."w3c-hr-time-1.0.1"
            sources."webidl-conversions-4.0.2"
            sources."whatwg-encoding-1.0.5"
            sources."whatwg-mimetype-2.3.0"
            sources."whatwg-url-6.5.0"
            sources."which-pm-runs-1.0.0"
            sources."wide-align-1.1.3"
            sources."wordwrap-1.0.0"
            sources."wrappy-1.0.2"
            sources."ws-5.2.2"
            sources."xml-name-validator-3.0.0"
            sources."xtend-4.0.1"
        ];

        buildInputs = [];
        meta = {
            description = "A language server for Bash";
            homepage = "https://github.com/mads-hartmann/bash-language-server#readme";
            license = "MIT";
        };
        production = true;
        bypassCache = true;
    };

}
