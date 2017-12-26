{ callPackage }:

callPackage ({ melpaBuild, lib }: melpaBuild {
  pname = "ocelot";
  version = "0.0.1";
  src = ./ocelot;
  packageRequires = [ ];
  meta = {
    license = lib.licenses.free;
  };
}) {}
