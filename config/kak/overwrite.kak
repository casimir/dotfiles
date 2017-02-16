
# overwrite a'la vim's R

def overwrite %{
  try %{
    hook buffer -group overwrite InsertChar .* %{exec <right><backspace>}
    map buffer insert <backspace> <left>
    hook buffer -group overwrite InsertEnd .* %{
      remove-hooks buffer overwrite
      unmap buffer insert <backspace> <left>
    }
    exec i
  }
}

