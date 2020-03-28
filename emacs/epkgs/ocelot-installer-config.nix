{ writeText,
  spacemacs, prelude, globalDistribution,
  elpaPinned, melpaPinned, orgPinned }:

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

  (defvar ocelot-installer-spacemacs-packages (list
  ${packageToPlist elpaPinned.cl-lib}
  ${packageToPlist elpaPinned.adaptive-wrap}
  ${packageToPlist orgPinned.org-plus-contrib}
  ${packageToPlist elpaPinned.spinner}))

  (defvar ocelot-installer-prelude-packages (list
  ${dependencyToPlist elpaPinned.let-alist}
  ${dependencyToPlist elpaPinned.seq}))

  (provide 'ocelot-installer-config)
  ''
