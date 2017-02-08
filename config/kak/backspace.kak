# make backspace remove trailing whitespace snapping at four characters

def backspace %{
    eval -no-hooks -itersel %{
        try %{
            exec -draft "\;<a-x>s^(.{%opt{tabstop}})*\K(\h{1,%opt{tabstop}}|.\h\K\h\h)$<ret>d"
        } catch %{
            exec -draft i<backspace><esc>
        }
    }
}

map global insert <backspace> '<a-;>:backspace<ret>'
map global insert <backtab> '<a-;>:backspace<ret>'
