{ stdenv, rustPlatform, pam }:

with rustPlatform;

buildRustPackage rec {
  name = "olman-${version}";
  version = "0.1.0";
  src = ./olman;

  buildInputs = [ pam ];

  cargoSha256 = "1wjzsvpakdw8n1pzkagbkap9gjxbpr5ivjyghsispsqzplrcb2w5";

  meta = with stdenv.lib; {
    description = "Ocelot Login Manager, a 'login' replacement";
  };
}
