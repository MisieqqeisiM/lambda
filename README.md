# lambda-parser
To execute do `stack run -- example.txt`. 
To see compiled representation do `stack run -- -compile example.txt`. 
To see AST `stack run -- -parse example.txt`. 

# Compilation
Language is compiled eagerly with an effort to share as many subtrees as possible. Subtrees are modified in-place if their reference count is 1 or they do not contain applied variable.

Numbers and tuples are represented natively in the parsed language. There are multiple arithmetic operator defined for numbers.

Boolean values are represented as fst and snd on a tuple.

Pattern matching is compiled into multiple function applications with chains of tuple indexing as arguments.

Ifs are lazy (they compile to `cond [fun _ => left, fun _ => right] []`)
