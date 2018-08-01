{ writeText, callPackage }:

callPackage ({ melpaBuild, lib }: melpaBuild {
  pname = "ocelot";
  version = "0.0.1";
  src = ./ocelot;
  packageRequires = [ ];
  recipe = writeText "recipe" ''
         (ocelot :fetcher git :url "localhost")
  '';
  meta = {
    license = lib.licenses.free;
  };
}) {}
