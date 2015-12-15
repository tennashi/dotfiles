xmodmap -e 'keycode 255=space'; xmodmap -e 'keycode 65=Shift_L'; xcape -e '#65=space'
export LANG=ja_JP.UTF-8

exec /usr/bin/xfce4-session
