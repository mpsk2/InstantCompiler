module LLVMConstants where

header :: [String]
header = [
	  "; print int declaration from runtime.bs"
	, ""
	, "; main function"
	, "declare void @printInt(i32)"
	, "define i32 @main() {"
	, "    ; content of main function"
	, ""
	]
	
footer :: [String]
footer = [
	  ""
	, "    ; end of main function"
	, "    ret i32 0"
	, "}"
	]
	
printConstant :: Integer -> [String]
printConstant constant = [
	  "    ; printing constant " ++ (show constant)
	, "    call void @printInt(i32 " ++ (show constant) ++ ")"
	, ""
	]
	
allocateVariable :: String -> [String]
allocateVariable name = [
	  "    ; allocating variable named " ++ name
	, "    %" ++ name ++ " = alloca i32"
	, ""
	]

