module LLVMConstants where

import Commons

header :: [String]
header = [
	  "; print int declaration from runtime.bs"
	, "declare void @printInt(i32)"
	, ""
	, "; main function"
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
	
printRegister :: Integer -> [String]
printRegister i = ["    call void @printInt(i32 %" ++ (show i) ++ ")"]

printVariable :: String -> Integer -> [String]
printVariable name i = [
	  "    ; printing variable " ++ name
	, "    %" ++ (show i) ++ " = load i32* %" ++ name ++ ", align 4" 
	, "    call void @printInt(i32 %" ++ (show i) ++ ")"
	, ""
	]
	
allocateVariable :: String -> [String]
allocateVariable name = [
	  "    ; allocating variable named " ++ name
	, "    %" ++ name ++ " = alloca i32, align 4"
	, ""
	]
	
loadVariable :: String -> Integer -> [String]
loadVariable name i = ["    %" ++ (show i) ++ " = load i32* %" ++ name ++ ", align 4"]

storeValue :: Value -> String -> [String]
storeValue (Value i) name = [
    "    ; storing value at variable " ++ name
  , "    store i32 " ++ (show i) ++ ", i32* %" ++ name ++ ", align 4"
  , ""
  ]
storeValue (Register inst i) name = inst ++ [
    "    ; storing register at variable " ++ name
  , "    store i32 %" ++ (show i) ++ ", i32* %" ++ name ++ ", align 4"
  , ""
  ]

arithmetic :: Integer -> Operation -> Value -> Value -> [String]
arithmetic i op (Value i1) (Value i2) = [
    "    %" ++ (show i) ++ " = " ++ (show op) ++ " i32 " ++ (show i1) ++ ", " ++ (show i2)
  ]
arithmetic i op (Register inst i1) (Value i2) = inst ++ [
    "    %" ++ (show i) ++ " = " ++ (show op) ++ " i32 %" ++ (show i1) ++ ", " ++ (show i2)
  ]
arithmetic i op a@(Value _) b@(Register _ _) = arithmetic i op b a
arithmetic i op (Register inst1 i1) (Register inst2 i2) = inst1 ++ inst2 ++ [
    "    %" ++ (show i) ++ " = " ++ (show op) ++ " i32 %" ++ (show i1) ++ ", %" ++ (show i2)
  ]