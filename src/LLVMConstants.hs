module LLVMConstants where

header :: [String]
header = [
	  "; print int declaration from runtime.bs"
	, "declare void @printInt(i32)"
	, "define i32 @main() {"
	]
	
footer :: [String]
footer = [
	  "    ret i32 0"
	, "}"
	]