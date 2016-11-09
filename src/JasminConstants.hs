module JasminConstants where

header :: String -> [String]
header className = [
    ".class public " ++ className
  , ".super java/lang/Object"
  , ""
  , ";"
  , "; standard initializer"
  , ".method public <init>()V"
  , "   aload_0"
  , ""
  , "   invokespecial java/lang/Object/<init>()V"
  , "   return"
  , ".end method"
  ]

mainHeader :: Integer -> Integer -> [String]
mainHeader stackLimit localsLimit = [
    ".method public static main([Ljava/lang/String;)V"
  , "   .limit stack " ++ (show stackLimit)
  , "   .limit locals " ++ (show localsLimit)  -- program arguments +1, but state starts counting from 1, so -1, so 0
  ]

mainFooter :: [String]
mainFooter = [
    "   return"
  , ".end method"
  ]
  
printInt :: [String] -> [String]
printInt instructions = [
    "   ; put System.out.println"
  , "   getstatic java/lang/System/out Ljava/io/PrintStream;"
  ] ++ instructions ++ [
  "   invokevirtual java/io/PrintStream/println(I)V"
  ]

data Operation = Add | Sub | Mul | Div deriving (Show, Eq)

saveToLocal :: Integer -> [String]
saveToLocal i
  | i <= 3    = ["   istore_" ++ (show i)]
  | otherwise = ["   istore " ++ (show i)]
  
loadLocal :: Integer -> [String]
loadLocal i
  | i <= 3    = ["   iload_" ++ (show i)]
  | otherwise = ["   iload " ++ (show i)]
  
-- supports only unsigned, there are no negative integers in syntax
intToStack :: Integer -> [String]
intToStack i
  | i <= 5    = ["   iconst_" ++ (show i)]
  | otherwise = ["   bipush " ++ (show i)]
  
operation :: [String] -> [String] -> Operation -> [String]
operation lhs rhs op = lhs ++ rhs ++ (op' op)
  where
    op' :: Operation -> [String]
    op' Add = ["   iadd"]
    op' Sub = ["   isub"]
    op' Mul = ["   imul"]
    op' Div = ["   idiv"]
