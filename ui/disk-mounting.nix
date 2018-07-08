{ config, pkgs, ... }:

{
  # TODO: use eudev on Linux

  config = {
    # enable setuid udevil and add it to the path
    programs.udevil.enable = true;
    environment.systemPackages = with pkgs; [
      udevil
    ];

    # configure devmon to work without udisks
    systemd.user.services.devmon = {
      description = "devmon automatic device mounting daemon";
      wantedBy = [ "default.target" ];
      path = [ "/run/wrappers" pkgs.procps pkgs.which ];
      serviceConfig.ExecStart = "${pkgs.udevil}/bin/devmon";
    };
  };
}
