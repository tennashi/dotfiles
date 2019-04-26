# fisherman install
if not functions -q fisher
    set -q XDG_CONFIG_HOME; or set XDG_CONFIG_HOME ~/.config
    curl https://git.io/fisher --create-dirs -sLo $XDG_CONFIG_HOME/fish/functions/fisher.fish
    fish -c fisher
end

set fish_key_bindings fish_vi_key_bindings

# color setting
set -g theme_nerd_fonts yes
set -g theme_color_scheme terminal2
set fish_color_error red
set fish_color_command green
set fish_color_quote blue
set fish_color_redirection blue
set fish_color_end yellow
set fish_color_param yellow
set fish_color_comment blue
set fish_color_match blue

