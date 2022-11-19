{
  description = "IO operations, using MonadIO & MonadError with AsIOError";

  inputs = {
    nixpkgs.url     = github:nixos/nixpkgs/be44bf67; # nixos-22.05 2022-10-15
    build-utils.url = github:sixears/flake-build-utils/r1.0.0.13;

    base1t.url          = github:sixears/base1t/r0.0.5.32;
    env-plus.url        = github:sixears/env-plus/r1.0.7.33;

    exited.url          = github:sixears/exited/r1.0.4.21;
    fpath.url           = github:sixears/fpath/r1.3.2.33;
    fstat.url           = github:sixears/fstat/r1.0.2.23;

    containers-plus.url = github:sixears/containers-plus/r0.0.10.34;
    monaderror-io.url   = github:sixears/monaderror-io/r1.2.5.18;
    more-unicode.url    = github:sixears/more-unicode/r0.0.17.11;
    natural.url         = github:sixears/natural/r0.0.1.13;
    tasty-plus.url      = github:sixears/tasty-plus/r1.5.2.21;
  };

  outputs = { self, nixpkgs, build-utils
            , base1t, containers-plus, env-plus, exited, fpath, fstat
            , monaderror-io, more-unicode, natural, tasty-plus
            }:
    build-utils.lib.hOutputs self nixpkgs "monadio-plus" {
##      deps = {
##        inherit base1t containers-plus env-plus exited fpath fstat monaderror-io
##                more-unicode natural tasty-plus;
##      };
      ghc = p: p.ghc8107; # for tfmt
##      overrideAttrs = pkgs: _: {
##        postConfigure = ''
##          substitute proto/MonadIO/Paths.hs src/MonadIO/Paths.hs \
##            --replace __gnugrep__ ${pkgs.gnugrep}
##        '';
##      };
      callPackage = { mkDerivation, lib, mapPkg, system
                    , base, base-unicode-symbols, bytestring, containers
                    , data-textual, deepseq, directory, exceptions, filelock
                    , filepath, gnugrep, lens, mtl, process, safe, tasty
                    , tasty-hunit, temporary, text, text-printer, unix
                    }:
        mkDerivation {
          pname = "monadio-plus";
          version = "2.5.1.39";
          src = ./.;
          isLibrary = true;
          isExecutable = true;
          libraryHaskellDepends = [
            base base-unicode-symbols bytestring containers data-textual deepseq
            directory exceptions filelock filepath lens mtl process safe
            tasty-hunit temporary text text-printer unix
          ] ++ mapPkg [
            base1t containers-plus env-plus exited fpath fstat monaderror-io
            more-unicode natural tasty-plus
          ];
          executableHaskellDepends = [ base data-textual ] ++ mapPkg [
            fpath monaderror-io more-unicode
          ];
          testHaskellDepends = [ base tasty ];
          description = "IO operations, using MonadIO & MonadError with AsIOError";
          license = lib.licenses.mit;
          postConfigure = ''
            substitute proto/MonadIO/Paths.hs src/MonadIO/Paths.hs \
              --replace __gnugrep__ ${gnugrep}
          '';
        };
    };
}
