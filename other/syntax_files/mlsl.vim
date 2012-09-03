" Vim systax file
" Language:  MLSL source code

if exists("b:current_syntax")
	finish
endif

syn keyword mlslTodo contained TODO FIXME XXX NOTE
syn match   mlslComment "//.*$" contains=mlslTodo
syn region  mlslComment start="(\*" end="\*)" contains=mlslComment,mlslTodo
syn keyword mlslKeyword contained and attr begin const end else fix fragment fun if in let match rec then vertex sampler shader when with
syn keyword mlslPrimType contained bool float int mat22 mat23 mat24 mat32 mat33 mat34 mat42 mat43 mat44 sampler2D samplerCube unit vec2 vec3 vec4
syn keyword mlslSemantics contained INPUT0 INPUT1 INPUT2 INPUT3 INPUT4 INPUT5 INPUT6 INPUT7 POSITION TEXCOORD0 TEXCOORD1 TEXCOORD2 TEXCOORD3
syn keyword mlslBoolConst contained true false
syn keyword mlslBuiltin contained min
syn match   mlslVariable "[a-z'_][A-Za-z0-9'_]*" contains=mlslKeyword,mlslPrimType,mlslBoolConst
syn match   mlslConstructor "[A-Z][A-Za-z0-9'_]*" contains=mlslSemantics
syn match   mlslVarying "[$][A-Za-z'_][A-Za-z0-9'_]*"
syn match   mlslNumber "[0-9][0-9]*([.][0-9]*)?([eE][+-]?[0-9][0-9]*)?"

let b:current_syntax = "mlsl"

hi def link mlslTodo          Todo
hi def link mlslComment       Comment
hi def link mlslKeyword       Keyword
hi def link mlslPrimType      Type
hi def link mlslSemantics     Special
hi def link mlslBuiltin       Special
hi def link mlslVariable      Identifier
hi def link mlslVarying       Identifier
hi def link mlslNumber        Number
hi def link mlslConstructor   Constant
hi def link mlslBoolConst     Constant
