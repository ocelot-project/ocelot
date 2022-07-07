{ stdenv, fetchFromGitHub, rustPlatform, pam }:

with rustPlatform;

buildRustPackage rec {
  name = "olman-${version}";
  version = "0.1.0";
  src = fetchFromGitHub {
    owner = "ocelot-project";
    repo = "olman";
    rev = "0703afaaf514d38d56f809909fd56b3b11eb52f4";
    sha256 = "0n9qv26lp7ylssghnpldv3gqj1v28csh2r1vcvwfy47c8z5sdfn3";
  };

  buildInputs = [ pam ];

  cargoSha256 = "sha256-bze/2m0FEiW9aqiGY4xJeDixWX5Lc8/AETbS5XHY0VU=";

  meta = with stdenv.lib; {
    description = "Ocelot Login Manager, a 'login' replacement";
    homepage = "https://github.com/ocelot-project/olman";
    maintainers = "The Ocelot Project";
  };
}
