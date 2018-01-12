# emacs2nix doesn't support EmacsWiki anymore, so info+ needs
# a custom derivation until it moves its source

{ callPackage }:

callPackage ({ elpaBuild, fetchurl, lib }: elpaBuild {
  pname = "hide-comnt";
  version = "20170223.739";
  src = fetchurl {
    url = "https://www.emacswiki.org/emacs/download/info+.el?revision=156";
    sha256 = "05150ypzgyk647gr565nbqaknf69kl083i27wzq0ippya20d74m1";
  };
  packageRequires = [];
  meta = {
    homepage = "https://www.emacswiki.org/emacs/info+.el";
    license = lib.licenses.free;
  };
}) {}
