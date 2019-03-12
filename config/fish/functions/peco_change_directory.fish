function peco_change_directory
  if test (count $argv) = 0
    set peco_flags --layout=bottom-up
  else
    set peco_flags --layout=bottom-up --query "$argv"
  end

  _peco_change_directory | peco $peco_flags | read foo

  if [ $foo ]
    builtin cd $foo
  else
    commandline ''
  end
end

function _peco_change_directory
  begin
    ghq list -p
    z -l | awk '{print $2}'
  end | sed -e 's/\/$//' | awk '!a[$0]++'
end

