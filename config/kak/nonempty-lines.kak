# Todo: rewrite without loops

def ensure-empty-line %{
    exec -no-hooks -draft \;<a-x><a-K>[^\n]<ret>
}

def _while-empty -params 1 %{
    try %{
        exec -no-hooks -draft \;Gg<a-k>(\n.*){3}<ret>
        exec -no-hooks -draft \;Ge<a-k>(\n.*){3}<ret>
        ensure-empty-line
        exec -no-hooks %arg{1}
        _while-empty %arg{1}
    }
}

def while-empty -params 1 %{ eval -itersel _while-empty %arg{1} }

def remove-adjacent-empty-line %{
    eval -no-hooks -itersel %{
        try %{
            eval -no-hooks -draft %{
                exec -no-hooks k
                ensure-empty-line
                exec -no-hooks d
            }
        } catch %{
            try %{
                eval -no-hooks -draft %{
                    exec -no-hooks j
                    ensure-empty-line
                    exec -no-hooks d
                }
            }
        }
    }
}

def remove-all-adjacent-empty-lines %{
    eval -no-hooks -draft %{
        eval -no-hooks -draft %{
            exec -no-hooks j
            while-empty d
        }
        eval -no-hooks -draft %{
            exec -no-hooks k
            while-empty dk
        }
    }
}
