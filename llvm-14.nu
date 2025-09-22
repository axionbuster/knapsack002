def --env putenvstr [key: string, value: string] {
 let prev = $env.$key? | default ""
 {$key: ($prev + $value)} | load-env
}

export-env {
 putenvstr "LDFLAGS" " -L/opt/homebrew/opt/llvm@14/lib"
 putenvstr "CPPFLAGS" " -I/opt/homebrew/opt/llvm@14/include"
 $env.PATH = $env.PATH | prepend "/opt/homebrew/opt/llvm@14/bin"
}
