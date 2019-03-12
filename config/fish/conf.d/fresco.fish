set -l bootstrap_file /home/yota/.local/share/fresco/repos/github.com/masa0x80/fresco/fresco.fish
if test -r $bootstrap_file
    source $bootstrap_file
else
    mkdir -p /home/yota/.local/share/fresco/repos/github.com/masa0x80/fresco
    git clone https://github.com/masa0x80/fresco /home/yota/.local/share/fresco/repos/github.com/masa0x80/fresco
end
