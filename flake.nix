{
  description = "hash - The Haskell Shell";
  inputs.flake-utils.url = "github:numtide/flake-utils";
  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system: rec {
      packages = rec {
        hash =
          with nixpkgs.legacyPackages."${system}";
          stdenv.mkDerivation {
            pname = "hash";
            version = "0.1.0";
            src = ./.;
            buildInputs = [ (haskellPackages.ghcWithPackages (p: with p; [
              process bytestring utf8-string
            ]) ) ];
            buildPhase = ''
              runHook preBuild
              ghc ./src/*.hs -o hash
              runHook postBuild
            '';
            installPhase = ''
              runHook preInstall
              mkdir -p $out/bin
              cp hash $out/bin/
              runHook postInstall
            '';
          };
        default = hash;
      };
      devShell =
        with nixpkgs.legacyPackages.${system};
        mkShell {
          packages = [ haskell-language-server ];
          inputsFrom = [ packages.default ];
        };
    });
}
