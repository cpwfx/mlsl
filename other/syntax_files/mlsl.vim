" Vim systax file
" Language:  MLSL source code

if exists("b:current_syntax")
	finish
endif

syn keyword mlslTodo contained TODO FIXME XXX NOTE
syn match   mlslComment "//.*$" contains=skimlTodo
syn region  mlslComment start="(\*" end="\*)" contains=skimlTodo
syn keyword mlslKeyword contained attr const fragment let vertex shader
syn keyword mlslPrimType contained bool float int mat44 unit vec2 vec3 vec4
syn keyword mlslSemantics contained POSITION
syn match   mlslVariable "[A-Za-z'_][A-Za-z0-9'_]*" contains=mlslKeyword,mlslPrimType,mlslSemantics
syn match   mlslVarying "[$][A-Za-z'_][A-Za-z0-9'_]*"
syn match   mlslNumber "[0-9][0-9]*([.][0-9]*)?([eE][+-]?[0-9][0-9]*)?"

let b:current_syntax = "mlsl"

hi def link mlslTodo          Todo
hi def link mlslComment       Comment
hi def link mlslKeyword       Keyword
hi def link mlslPrimType      Type
hi def link mlslSemantics     Special
hi def link mlslVariable      Identifier
hi def link mlslVarying       Identifier
hi def link mlslNumber        Number
