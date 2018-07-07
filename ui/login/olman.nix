{ stdenv, rustPlatform, pam }:

with rustPlatform;

buildRustPackage rec {
  name = "olman-${version}";
  version = "0.1.0";
  src = ./olman;

  buildInputs = [ pam ];

  cargoSha256 = "1005iig9m4jfb6crhd4c54q91v2fvk433dkk5l543zqvfhvv4ddq";

  meta = with stdenv.lib; {
    description = "Ocelot Login Manager, a 'login' replacement";
  };
}
