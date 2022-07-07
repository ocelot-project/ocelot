{ callPackage, writeText, trivialBuild }:

callPackage ({ lib }: trivialBuild {
  pname = "ocelot";
  version = "0.0.1";
  src = ./ocelot;
  packageRequires = [ ];
  meta = {
    license = lib.licenses.free;
  };
}) {}
