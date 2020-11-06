{ writeText, spacemacs, prelude, globalDistribution, epkgs }:

let
packageToPlist =
  epkg: ("(list 'name '${epkg.pname} 'path \"${epkg}\" 'dependency nil)");

dependencyToPlist =
  epkg: ("(list 'name '${epkg.pname} 'path \"${epkg}\" 'dependency t)");

in

writeText "ocelot-installer-config.el" ''
  (defvar ocelot-installer-spacemacs-path "${spacemacs}")
  (defvar ocelot-installer-prelude-path "${prelude}")

  (defvar ocelot-global-distribution "${globalDistribution}")
  (defvar ocelot-user-distributions '())

  (provide 'ocelot-installer-config)
  ''
