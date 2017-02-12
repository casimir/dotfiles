# make backspace remove spaces snapping at four characters

def backspace %{
    eval -no-hooks -itersel %{
        try %{
            # corner case: at the start of line
            exec -draft ';<a-k>^.\z<ret>i<backspace>' 
        } catch %{ try %{
            exec -draft ";hGhs^([^\t]{ %opt{tabstop} })*\K[ ]{1, %opt{tabstop} }\z<ret>d"
        } catch %{
            # fall back to normal backspace if there are no spaces to remove
            exec -draft i<backspace>
        }}
    }
}

map global insert <backspace> '<a-;>:backspace<ret>'
map global insert <backtab>   '<a-;>:backspace<ret>'
