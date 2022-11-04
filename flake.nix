{
  description = "IO operations, using MonadIO & MonadError with AsIOError";

  inputs = {
    nixpkgs.url     = "github:nixos/nixpkgs/be44bf67"; # nixos-22.05 2022-10-15
    build-utils.url = "github:sixears/flake-build-utils/r1.0.0.6";

    # we suspect some combination of base1t containers-plus & fpath is causing
    # the blowout
    # all - blowout
    # base1t+env-plus+exited+fpath+fstat - blowout
    # base1t+env-plus+fpath+fstat - no blowout
    # base1t+exited+fpath+fstat - no blowout
    # base1t+env-plus+exited+fpath - no blowout
    # env-plus+exited+fpath+fstat - no blowout
    # base1t+env-plus+exited+fstat - no blowout
    base1t.url          = "github:sixears/base1t/r0.0.5.10";
#    containers-plus.url = "github:sixears/containers-plus/r0.0.10.10";
    env-plus.url        = "github:sixears/env-plus/r1.0.7.10";

    exited.url          = "github:sixears/exited/r1.0.4.10";
    fpath.url           = "github:sixears/fpath/r1.3.2.10";
    fstat.url           = "github:sixears/fstat/r1.0.2.8";

#    monaderror-io.url   = "github:sixears/monaderror-io/r1.2.5.8";
#    more-unicode.url    = "github:sixears/more-unicode/r0.0.17.6";
#    natural.url         = "github:sixears/natural/r0.0.1.6";
#    tasty-plus.url      = "github:sixears/tasty-plus/r1.5.2.8";
  };

  outputs = { self, nixpkgs, build-utils
            , base1t
            # , containers-plus
            , env-plus
            , exited
            , fpath
            , fstat
            # , monaderror-io, more-unicode, natural, tasty-plus
            }:
    build-utils.lib.hOutputs self nixpkgs "monadio-plus" {
      deps = {
        inherit
          base1t
          # containers-plus
          env-plus
          exited
          fpath
          fstat
          # monaderror-io more-unicode natural tasty-plus
        ;
      };
      ghc = p: p.ghc8107; # for tfmt
    };
}
