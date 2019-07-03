{ stdenv, rustPlatform, pam }:

with rustPlatform;

buildRustPackage rec {
  name = "olman-${version}";
  version = "0.1.0";
  src = ./olman;

  buildInputs = [ pam ];

  cargoSha256 = "1l31xkhnnzpfvijjn3phgh44bd8nsljp415lrb7gmf18p5zksf6j";

  meta = with stdenv.lib; {
    description = "Ocelot Login Manager, a 'login' replacement";
  };
}
