
# marks: jump to and save sels
def marks -params 0..1 %{
  info -title "marks%arg{1}" %{l: load
s: save
a: append
u: union}
  on-key %{%sh{
    key=""
    case "$kak_key" in
      l) key=z ;;
      s) key=Z ;;
      a) key='<a-Z>' ;;
      u) key='<a-z>' ;;
      [a-zA-Z]) echo marks '%{"'"$kak_key"'}' ;;
      *) echo echo unbound "$kak_key" ;;
    esac
    if [[ "$key" ]]; then
        echo exec -no-hooks -save-regs "''" '%{'"$1""$key"'}'
    fi
  }}
}

