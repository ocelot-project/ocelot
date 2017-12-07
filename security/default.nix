{ config, pkgs, ...}:

{
  programs.gnupg.agent = {
    enable = true;
    enableSSHSupport = true;
  };

  environment.systemPackages = with pkgs; [
    pass
    gnupg
  ];
}
