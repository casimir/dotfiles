
# Reload kakrc and .kak when saving.
# Adds -allow-override to definitions (unless they seem to be python defs!)

rmhooks global reload_kak
hook -group reload_kak global BufWritePost (.*kakrc|.*\.kak) %{
  rmhooks global kakrc
  decl -hidden str reload_file
  %sh{
    tmp=$(mktemp /tmp/kak-source.XXXXXX)
    echo set buffer reload_file $tmp
  }
  write %opt{reload_file}
  %sh{
    sed -i 's/^def \([^:]*\)$/def -allow-override \1/' $kak_opt_reload_file
    sed -i 's/^plug/#/' $kak_opt_reload_file
  }
  source %opt{reload_file}
  echo Reloaded %val{bufname}
  %sh{ rm $kak_opt_reload_file }
}

