{ config, lib, pkgs, ...}:

with lib;

let
  cfg = config.ocelot.ui;
in
{
  imports = [
    ./keyboard.nix
    ./locker.nix
    ./login
    ./gui.nix
    ./disk-mounting.nix
  ];

  options.ocelot.ui = {
    graphical = mkOption {
      type = types.bool;
      default = true;
      description = ''
        If enabled, Ocelot is configured for graphical operation. Otherwise,
        Ocelot uses a text-based user interface.
      '';
    };

    workspaces = mkOption {
      type = types.attrsOf (types.listOf types.int);
      default = {};
      description = ''
      An attribute set whose keys are RandR display names, and values are
      lists of EXWM workspace numbers assigned to each display.
      '';
    };
  };

  config = {
    ocelot.ui.login.sessions = mkOrder 500 [
      {
       name = "Textmode Emacs";
       command = "exec ${pkgs.ocelotEmacs}/bin/emacs -nw";
      }

      {
        name = "Textmode Shell";
      }
    ];

    # Emacs vterm shell configuration
    programs.bash.interactiveShellInit = ''
vterm_printf(){
  if [ -n "$TMUX" ]; then
    # Tell tmux to pass the escape sequences through
    # (Source: http://permalink.gmane.org/gmane.comp.terminal-emulators.tmux.user/1324)
    printf "\ePtmux;\e\e]%s\007\e\\" "$1"
  elif [ "$${TERM%%-*}" = "screen" ]; then
    # GNU screen (screen, screen-256color, screen-256color-bce)
    printf "\eP\e]%s\007\e\\" "$1"
  else
    printf "\e]%s\e\\" "$1"
  fi
}

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
  function clear(){
    vterm_printf "51;Evterm-clear-scrollback";
    tput clear;
  }

  vterm_prompt_end(){
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
  }
  PS1=$PS1'\[$(vterm_prompt_end)\]'

  emacs-eval() {
    local vterm_elisp
    vterm_elisp=""
    while [ $# -gt 0 ]; do
      vterm_elisp="$vterm_elisp""$(printf '"%s" ' "$(printf "%s" "$1" | sed -e 's|\\|\\\\|g' -e 's|"|\\"|g')")"
      shift
    done
    vterm_printf "51;E$vterm_elisp"
  }

  find-file() {
    emacs-eval find-file "$(realpath "$${@:-.}")"
  }
fi
    '';

    programs.bash.promptInit = ''
# Provide a nice prompt if the terminal supports it.
if [ "$TERM" != "dumb" -o -n "$INSIDE_EMACS" ]; then
  PROMPT_COLOR="1;31m"
  let $UID && PROMPT_COLOR="1;32m"
  if [ -n "$INSIDE_EMACS" -o "$TERM" == "eterm" -o "$TERM" == "eterm-color" ]; then
    # Emacs term mode doesn't support xterm title escape sequence (\e]0;)
    PS1="\n\[\033[$PROMPT_COLOR\][\u@\h:\w]\\$\[\033[0m\] "
  else
    PS1="\n\[\033[$PROMPT_COLOR\][\[\e]0;\u@\h: \w\a\]\u@\h:\w]\\$\[\033[0m\] "
  fi
  if test "$TERM" = "xterm"; then
    PS1="\[\033]2;\h:\u:\w\007\]$PS1"
  fi
fi

if [[ "$INSIDE_EMACS" = 'vterm' ]]; then
  vterm_prompt_end(){
    vterm_printf "51;A$(whoami)@$(hostname):$(pwd)"
  }
  PS1=$PS1'\[$(vterm_prompt_end)\]'
fi
  '';
  };
}
