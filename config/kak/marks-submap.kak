
# marks: jump to and save sels
def marks -params 0..1 %{
  info -title "marks%arg{1}" %{(g)et            z
(s)et            Z
(a)dd            <a-Z>
(u)nion          <a-z>
(o)nly selection <space>
(r)everse orient <a-;>
(f)orward orient <a-:>
(b)ackward orient
(;)thin selection}
  on-key %{%sh{
    key=""
    case "$kak_key" in
      g) key=z ;;
      s) key=Z ;;
      a) key='<a-Z>' ;;
      u) key='<a-z>' ;;
      o) key='<space>' ;;
      r) key='<a-;>' ;;
      f) key='<a-:>' ;;
      b) key='<a-:><a-;>' ;;
      ';') key=';' ;;
      [a-zA-Z]) echo marks '%{"'"$kak_key"'}' ;;
      *) echo echo unbound "$kak_key" ;;
    esac
    if [[ "$key" ]]; then
        echo exec -no-hooks -save-regs "''" '%{'"$1""$key"'}'
    fi
  }}
}

