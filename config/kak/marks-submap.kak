
# marks: jump to and save sels
def marks -params 0..1 %{
  info -title "marks%arg{1}" %{l: load     z
s: save     Z
a: append   <a-Z>
u: union    <a-z>
o: only selection}
  on-key %{%sh{
    key=""
    case "$kak_key" in
      l) key=z ;;
      s) key=Z ;;
      a) key='<a-Z>' ;;
      u) key='<a-z>' ;;
      o) key='<space>' ;;
      [a-zA-Z]) echo marks '%{"'"$kak_key"'}' ;;
      *) echo echo unbound "$kak_key" ;;
    esac
    if [[ "$key" ]]; then
        echo exec -no-hooks -save-regs "''" '%{'"$1""$key"'}'
    fi
  }}
}

