
def Z-submap %{
    info Z
    on-key %{z-handle %val{key} 0 true}
}

def -params 1 z-submap %{
    decl -hidden int zcount %arg{1}
    info z
    on-key %{z-handle %val{key} %opt{zcount} false}
}

def z-handle -params 3 %{
  %sh{
    repeat=$3
    z=""
    Z=""
    case "$1" in
      H)            echo exec Gi  ;;
      h) Z="exec vh"; z="exec gi" ;;
      t) Z="exec vj"; z="exec vt" ;;
      n) Z="exec vk"; z="exec ge" ;;
      s) Z="exec vl"  ;;

      c) echo exec vc ;;
      b) echo exec vb ;;
      N) echo exec Ge ;;

      6) echo exec gt ;;
      7) echo exec gc ;;
      8) echo exec gb ;;

      w) echo exec '<c-f>gc' ;;
      v) echo exec '<c-b>gc' ;;
      W) echo exec '<c-d>gc' ;;
      V) echo exec '<c-u>gc' ;;

      f) Z="exec <a-n>vc" ;;
      g) Z="exec nvc"
         if [ "$2" -eq 0 ]; then
           z="exec gg";
         else
           z="exec $kak_opt_zcount g";
         fi
         ;;
      G) if [ "$2" -eq 0 ]; then
           echo exec Gg;
         else
           echo "exec $kak_opt_zcount G";
         fi
         ;;
      e) echo exec gl ;;
      E) echo exec Gl ;;
      a) echo exec gh ;;
      A) echo exec Gh ;;
      k) echo exec '<A-K>' ;;
      o) echo exec '<A-a>' ;;
      i) echo exec '<A-i>' ;;
      q) echo exec ':q<ret>' ;;

      r) echo exec ':new-client-here<space>F1<ret>' ;;
      R) echo exec ':new-client-here<space>F2<ret>' ;;

      F) echo exec gf ;;
      u) echo exec 'g.' ;;
      '`') echo exec 'ga' ;;
      m) repeat="true" ;;
      *) repeat="false" ;;
      # d still unused
    esac
    if [[ "$repeat" == "true" ]]; then
      echo "$Z"
      echo info Z
      echo on-key %{z-handle %val{key} 0 true}
    else
      echo "$z"
    fi
  }
}

map global normal z ':z-submap<space>%val{count}<ret>'
map global normal Z ':Z-submap<ret>'

def -params 1 new-client-here %{
  %sh{
    echo "new exec :buffer <space> $kak_buffile <ret> $kak_cursor_line g :%sh{ xdotool <space> key <space> alt+$1 }<ret>"
  }
}

