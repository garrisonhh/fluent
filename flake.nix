{
  inputs = {
    nixpkgs.url = github:NixOs/nixpkgs/nixos-23.05;
    zig = {
      url = github:mitchellh/zig-overlay;
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, zig, nixpkgs }:
    let
      name = "fluent-parser";
      system = "x86_64-linux";

      pkgs = (import nixpkgs) {
        inherit system;
      };
      zigpkg = zig.packages.${system}."0.11.0";

      inputs = [ zigpkg ];

      # create a derivation for the build with some args for `zig build`
      makePackage = buildArgs:
        let
          argv =
            with pkgs.lib.strings;
            concatStrings (intersperse " " buildArgs);
        in
        pkgs.stdenv.mkDerivation {
          inherit name;
          src = self;

          buildInputs = inputs;
          buildPhase = ''
            export HOME=$NIX_BUILD_TOP
            zig build ${argv}
          '';

          installPhase = ''
            mkdir $out
            cp -r zig-out/* $out/
          '';

          outputs = [ "out" ];
        };

      packages = {
        default = makePackage [ ];
        debug = makePackage [ "-Doptimize=Debug" ];
        release = makePackage [ "-Doptimize=ReleaseFast" ];
      };
    in
    {
      devShells.${system}.default = packages.debug;
      packages.${system} = packages;
      formatter.${system} = pkgs.nixpkgs-fmt;
    };
}
