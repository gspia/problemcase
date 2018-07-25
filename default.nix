{}:
(import ./reflex-platform {}).project ({ pkgs, ... }: {
  packages = {
    example = ./.;
  };

  shells = {
    ghc   = [ "example" ];
    ghcjs = [ "example" ];
  };
})
