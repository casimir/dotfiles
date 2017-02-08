# make backspace remove trailing whitespace snapping at four characters

def backspace %{
    eval -no-hooks -itersel %{
        try %{
            exec -draft '\;<a-x>s^(.{4})*\K(\h{1,4}|.\h\K\h\h)$<ret>d'
        } catch %{
            exec -draft i<backspace><esc>
        }
    }
}

map global insert <backspace> '<a-;>:backspace<ret>'
