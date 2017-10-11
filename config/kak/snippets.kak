try %{ decl str snippets }

hook -group kakrc global InsertChar \n %{
    eval -draft -no-hooks %{
        exec 'k<a-x>H'
        %sh{
            (while read line; do
                proc() {
                    short=$1
                    shift
                    [ -n "$short" ] && (
                        echo "reg s '$short'"
                        echo "reg r \"$@\""
                        echo "try %{ exec -draft \"<a-k>\A%reg{s}\z<ret>c%reg{r}<esc>\" }"
                    )
                }
                proc $line
            done) <<<"$kak_opt_snippets"
        }
    }
}

