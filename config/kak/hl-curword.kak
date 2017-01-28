
# Highlight current word, from mawww

decl -hidden regex curword
face CurWord +b

def -hidden -allow-override _update_curword %{
    eval -no-hooks -draft %{ try %{
        exec <space><a-i>w <a-k>\`\w+\'<ret>
        set buffer curword "\b\Q%val{selection}\E\b"
    } catch %{
        set buffer curword ''
    } }
}

hook global -group kakrc NormalKey .* _update_curword
