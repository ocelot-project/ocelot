# emacs2nix doesn't support EmacsWiki anymore, so help-fns+ needs
# a custom derivation until it moves its source

{ callPackage }:

callPackage ({ elpaBuild, fetchurl, lib }: elpaBuild {
  pname = "help-fns+";
  version = "20170223.733";
  src = fetchurl {
    url = "https://www.emacswiki.org/emacs/download/help-fns+.el?revision=134";
    sha256 = "0n7sdzvplcb0zivpjq9x75kaid59yxr7sl85w7z99irx3kgpy9y4";
  };
  packageRequires = [];
  meta = {
    homepage = "https://www.emacswiki.org/emacs/help-fns+.el";
    license = lib.licenses.free;
  };
}) {}
