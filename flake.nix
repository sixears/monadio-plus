{
  description = "IO operations, using MonadIO & MonadError with AsIOError";

  inputs = {
    nixpkgs.url     = "github:nixos/nixpkgs/be44bf67"; # nixos-22.05 2022-10-15
    build-utils.url = "github:sixears/flake-build-utils/r1.0.0.3";

    base1t.url          = "github:sixears/base1t/r0.0.5.2";
    containers-plus.url = "github:sixears/containers-plus/r0.0.10.3";
    env-plus.url        = "github:sixears/env-plus/r1.0.7.2";
    exited.url          = "github:sixears/exited/r1.0.4.3";
    fpath.url           = "github:sixears/fpath/r1.3.2.2";
    fstat.url           = "github:sixears/fstat/r1.0.2.1";
    monaderror-io.url   = "github:sixears/monaderror-io/r1.2.5.2";
    more-unicode.url    = "github:sixears/more-unicode/r0.0.17.1";
    natural.url         = "github:sixears/natural/r0.0.1.1";
    tasty-plus.url      = "github:sixears/tasty-plus/r1.5.2.1";
  };

  outputs = { self, nixpkgs, flake-utils, build-utils
            , base1t, containers-plus, env-plus, exited, fpath, fstat
            , monaderror-io, more-unicode, natural, tasty-plus }:
    build-utils.lib.hOutputs self nixpkgs "monadio-plus" {
      deps = {
        inherit base1t containers-plus env-plus exited fpath fstat monaderror-io
                more-unicode natural tasty-plus;
      };
      ghc = p: p.ghc8107; # for tfmt
    };
}
