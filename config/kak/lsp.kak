
def lsp-enable %{
    # todo: fail if jq is missing
    declare-option str lsp_dir
    declare-option str lsp_servers
    declare-option str lsp_running
    declare-option str lsp_fifo
    declare-option str lsp_jq_handler ''
    %sh{
        dir=$(mktemp -d "${TMPDIR:-/tmp}"/kak-lsp.XXXXXXXX)
        servers="$dir"/servers.json
        running="$dir"/running.json
        echo {} > $servers
        echo {} > $running
        echo "
            set-option global lsp_dir '$dir'
            set-option global lsp_servers '$servers'
            set-option global lsp_running '$running'
        "
    }

    def lsp-register -params 2 %{
        %sh{
            echo "$(jq '.+{($filetype):$cmd}'
                       --arg filetype "$kak_arg_1"
                       --arg cmd "$kak_arg_2"
                       "$kak_opt_lsp_servers")" >
                       "$kak_opt_lsp_servers"
        }
    }

    def lsp-for-filetype %{
        # lookup in the magic json object which maps filetypes to fifos
        # could have server for each filetype AND working directory
        %sh{
            fifo=$(jq '.[$filetype].fifo'
                      --arg filetype "$kak_opt_filetype"
                      "$kak_opt_lsp_running")
            if [ "$fifo" != null ]; then
                echo "
                    set-option window lsp_fifo '$fifo'
                "
            else
                cmd=$(jq '.[$filetype]'
                      --arg filetype "$kak_opt_filetype"
                      "$kak_opt_lsp_servers")
                if [ "$cmd" ]; then
                    echo "
                        lsp-start-server '$kak_opt_filetype' '$cmd'
                    "
                fi
            fi
        }
    }

    def -hidden lsp-start-server -params 2 %{
        %sh{
            filetype="$kak_arg_1"
            cmd="$kak_arg_2"
            prefix="$lsp_dir/$kak_opt_filetype"
            fifo="$prefix.fifo"
            mkfifo "$fifo"

            function send_message() {
                echo -e -n "Content-length: $(echo -n "$1" | wc -c)\r\n\r\n$1"
            }

            function read_message() {
                while true; do
                    read -r line
                    byte_count=$(expr "${line}" : 'Content-Length: \(..*\).')
                    test "$byte_count" && break
                done
                while true; do
                    read -r line
                    test "$line" && continue
                done
                dd bs="$byte_count" count=1 2>/dev/null
            }

            function preprocess() {
                # initialize:
                send_message '{
                    "jsonrpc":"2.0",
                    "method":"initialize",
                    "params":{
                        "processId":'"$session"',
                        "rootUri":"file://'"$PWD"'",
                        "rootPath":"'$PWD'",
                        "capabilities":{}}}'
                id=0

                while true; do
                    # add $id to this too:
                    # calculate pos uri file, also first send current contents if we have .contents
                    inmsg=$(read_message)
                    outmsg=$(jq ... )
                    make_message "$outmsg" > "$infifo"
                    echo "$inmsg" > "$kak_opt_lsp_dir/$kak_opt_filetype-closure-$id.json"
                    id=$(($id + 1))
                done
            }

            function postprocess() {
                while true; do
                    reply=$(read_message)
                    id=$(echo "$reply" | jq '.id')
                    closure_file="$kak_opt_lsp_dir/$kak_opt_filetype-closure-$id.json"
                    rm "$closure_file"
                    merged=$(echo "$reply" | jq -s '{"closure":.[0],"reply":.[1]}' "$closure_file")
                    tmp="$kak_opt_lsp_dir/$kak_opt_filetype-$id.kak"
                    echo "$merged" | jq "'""$kak_lsp_jq_handler"' error("no handler for message")'"'") > $tmp
                    client=$(echo "$merged" | jq '.closure.common.client')
                    echo "eval -client $client %{source '$tmp'; %sh{rm '$tmp'}}" | kak -p "$kak_session"
                done
            }

            {
                tail -f "$fifo"
                | preprocess 2>"$prefix.pre.err"
                | "$cmd" 2>"$prefix.server.err"
                | postprocess >"$prefix.post.out" 2>"$prefix.post.err"
            } 2>&1 >/dev/null </dev/null &
            pid=$!

            echo "$(jq '.+{($filetype):{fifo:$fifo,pid:[$pid]}}'
                       --arg filetype "$filetype"
                       --arg fifo "$fifo"
                       --arg pid "$pid"
                       "$kak_opt_lsp_running")" >
                       "$kak_opt_lsp_running"
                } &
            ) 2>&1 >/dev/null </dev/null &
            echo "
                set-option window lsp_fifo '$fifo'
            "
        }
    }

    def lsp-send-buffer-contents %{
        eval -draft %{
            exec \%
            expand-write "'%opt{lsp_fifo}'" '{"type":"contents","common":{"file":%val{buffile},"timestamp":%val{timestamp},"client":%val{client},"contents":%val{selection}}}}'
        }
    }

    def -hidden lsp-send-message -params 3 %{
        eval -save-regs b %{
            set-register b %arg{3}
            expand-write "'%opt{lsp_fifo}'" '{"type":"message","specific":%reg{b},"common":{"line":%val{cursor_line},"column":%val{cursor_column},"file":%val{buffile},"timestamp":%val{timestamp},"client":%val{client}}}'
        }
    }

    hook -group lsp global WinSetOption ^filetype= lsp-for-filetype

    def lsp-on-reply -params 2 %{
        set -add global lsp_jq_handler "if .method == \"%arg{1}\" then %arg{2} else "
    }

    def lsp-hover -params 0..1 %{
        lsp-send-buffer-contents
        lsp-send-message 'textDocument/hover' %opt{langserver} "\"%arg{1}\""
    }

    lsp-on-reply 'textDocument/hover' %{
        "info -placement above -anchor \(.cursor_line).\(cursor_column) \(@json .contents.value)"
    }
}

#lsp-register python pyls

def expand -params 1 %{
    eval -no-hooks -draft %{
        try %{
            edit -scratch *expand*
        }
    }
    eval -save-regs s %{
        eval -no-hooks -buffer *expand* %{
            reg '"' "%arg{1}"
            exec -no-hooks '%R'
            make-search-and-replace-script
        }
        eval -no-hooks "%reg{s}"
    }
}

def expand-write -params 2 %{
    expand %arg{2}
    eval -no-hooks -buffer *expand* %{
        write %arg{1}
        delete-buffer!
    }
}

def make-search-and-replace-script %{
    reg s ''
    exec -no-hooks '%s%\w*?\{.*?\}<ret>'
    eval -no-hooks -itersel %{
        reg s "replace-eval '%val{selection}' %val{selection};%reg{s}"
    }
}

def replace-eval -params 2 %{
    eval -no-hooks -save-regs qv %{
        set-register q "%arg{1}"
        set-register v "%arg{2}"
        eval -no-hooks -buffer *expand* %{
            exec -no-hooks -save-regs '' '/\Q<c-r>q<ret>"v<a-R>Zi"<esc>'
            try %{ exec -no-hooks 'zs["\\]<ret>i\<esc>' }
            try %{ exec -no-hooks 'zs\n<ret>c\n<esc>' }
            try %{ exec -no-hooks 'zs\t<ret>c\t<esc>' }
            exec -no-hooks 'za"<esc>'
        }
    }
}

