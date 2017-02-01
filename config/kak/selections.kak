
def select-all-focus-closest %{
    exec '%s<ret>' :focus-closest-to-line <space> %val{cursor_line} <ret>
    print-selection-info
}

def -params 1 focus-closest-to-line %{ %sh{
python - $1 $kak_selection_desc $kak_selections_desc <<PY
import sys
goto, current, all = sys.argv[1:]
all = all.split(':')
def key(t):
    desc = t[1]
    return abs(int(goto) - int(desc.split('.')[0]))
best_index, _ = min(enumerate(all), key=key)
current_index = all.index(current)
rots = best_index - current_index
if rots < 0: rots += len(all)
if rots != 0:
    print("exec {}'".format(rots))
PY
}}

def print-selection-info %{ %sh{
IFS=':' read -ra sels <<< "$kak_selections_desc"
for i in "${!sels[@]}"; do
   if [[ "${sels[$i]}" = "${kak_selection_desc}" ]]; then
       echo "echo selection $((i+1))/${#sels[@]}";
       break;
   fi
done
}}

hook global -group kakrc NormalKey .*(['snNCzZ]).* print-selection-info
hook global -group kakrc NormalKey [sS] %{
    try %{remove-hooks global once}
    # NormalBegin is not triggered when the prompt is finished,
    # but just before prompt launches (?)
    hook global -group once NormalBegin .* %{
        print-selection-info
        try %{remove-hooks global once}
    }
}

hook global -group kakrc NormalKey .*[/?nN*].* highlight-search

def highlight-search %{
  noh
  try %{
    addhl dynregex '%reg{/}' 0:+u 1:+i 2:+i
  }
}

def noh %{
  rmhl dynregex_\%reg{<slash>}
}

