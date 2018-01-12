# emacs2nix doesn't support EmacsWiki anymore, so hide-comnt needs
# a custom derivation until it moves its source

{ callPackage }:

callPackage ({ elpaBuild, fetchurl, lib }: elpaBuild {
  pname = "hide-comnt";
  version = "20170223.739";
  src = fetchurl {
    url = "https://www.emacswiki.org/emacs/download/hide-comnt.el?revision=25";
    sha256 = "1shkq45vm60nh2kkvf284nck8jwxh7f7m4c5d53k66mxn214h53m";
  };
  packageRequires = [];
  meta = {
    homepage = "https://www.emacswiki.org/emacs/hide-comnt.el";
    license = lib.licenses.free;
  };
}) {}
