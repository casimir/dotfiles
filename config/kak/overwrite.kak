
# overwrite a'la vim's R

def overwrite %{
  try %{
    hook buffer -group overwrite InsertChar .* %{exec <right><backspace>}
    map buffer insert <backspace> <left>
    hook buffer -group overwrite InsertEnd .* %{
      remove-hooks buffer overwrite
      map buffer insert <backspace> <backspace>
    }
    exec i
  }
}

