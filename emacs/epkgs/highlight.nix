# emacs2nix doesn't support EmacsWiki anymore, so highlight needs
# a custom derivation until it moves its source

{ callPackage }:

callPackage ({ elpaBuild, fetchurl, lib }: elpaBuild {
  pname = "highlight";
  version = "20170702.732";
  src = fetchurl {
    url = "https://www.emacswiki.org/emacs/download/highlight.el?revision=149";
    sha256 = "0kh72fmqsha25rz0g3ff983badh20clsig7blrhvl8c4bv3sqs56";
  };
  packageRequires = [];
  meta = {
    homepage = "https://www.emacswiki.org/emacs/highlight.el";
    license = lib.licenses.free;
  };
}) {}
