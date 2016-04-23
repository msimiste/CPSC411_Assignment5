module IRDataType where

data I_prog  = IPROG    ([I_fbody],Int,[I_stmt]) deriving (Eq,Show,Ord, Read)
    --    a program node consists of 
    --   (a) the list of functions declared
    --   (b) the number of local variables
    --   (c) the body: a list of statements
   
    
data I_fbody = IFUN (String,[I_fbody],Int,Int,[I_stmt]) deriving (Eq,Show,Ord, Read)
    --    a function node consists of 
    --   (a) the label given to the function
    --   (b) the list of local functions declared
    --   (c) the number of local variables
    --   (d) the number of arguments
    --   (e) the body: a list of statements

data I_stmt = IASS      (Int,Int,I_expr) --(level,offset,expr)
            | IWHILE    (I_expr,I_stmt)
            | ICOND     (I_expr,I_stmt,I_stmt)
            | IREAD_I   (Int,Int)
            | IPRINT_I  I_expr
            | IREAD_B   (Int,Int)
            | IPRINT_B  I_expr
            | IRETURN   I_expr
            | IBLOCK    ([I_fbody],Int,[I_stmt])
            deriving (Eq,Show,Ord, Read)
         -- a block consists of 
         -- (a) a list of local functions
         -- (b) the number of local varibles declared
         -- (c) the body: a lst of statements
         
data I_expr = INUM      Int
            | IBOOL     Bool
            | IID       (Int,Int) --(level,offset)
            | IAPP      (I_opn,[I_expr])
            deriving (Eq,Show,Ord, Read)
         --   isize(<level>,<offset>,<which dimension>)
         --   level and offset identify which array the last integer 
         --   tells you which dimension you want to look at!!

            
data I_opn = ICALL      (String,Int) --(label,level)
           | IADD | IMUL | ISUB | IDIV | INEG
           | ILT  | ILE  | IGT  | IGE  | IEQ 
           | INOT | IAND | IOR
           deriving (Eq,Show,Ord, Read)
