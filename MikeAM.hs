
module MikeAM where
import IRDataType
import Text.Show.Pretty    
    
--registers 
sp = "%sp" -- stack pointer
fp = "%fp" -- frame pointer
cp = "%cp" -- code pointer
hp = "%hp" -- heap pointer

{-
VALUE F
VALUE I
VALUE B
VALUE C
VOID
STACK PTR
HEAP PTR
CODE PTR-}

-- Frame pointer instructions:
loadR  reg = "LOAD_R " ++ reg 
storeR reg = "STORE_R " ++ reg

--Code pointer instructions:
jump lbl = "JUMP " ++ lbl --Sets the %cp to the value of the code label
jumpS = "JUMP_S"  --Removes the code pointer on top of the stack and sets %cp to this value advanced by one step.
jumpC lbl = "JUMP_C " ++ lbl --Removes the boolean value on top of the stack and if it is FALSE sets %cp to the value of the code label.
jump0 offset = "JUMP_O " ++ offset -- Removes the (positive) integer on top of the stack and moves the code pointer that number of steps ahead: 1= next instruction 2 = skip instruction 3 = skip 2 instructions
halt = "HALT" -- End of execution


-- Data manipulation on stack:
--loadf val = "LOAD_F" ++ show(val) -- Load a constant float/real number onto the stack.
loadI num = "LOAD_I " ++ show(num) -- Load a constant integer onto the stack.
loadB val = "LOAD_B " ++ val --load a constant boolean onto the stack.
loadO offset = "LOAD_O " ++ show(offset) --Removes the stack pointer 
loadOS num offset = "LOAD_OS " ++ show(num) ++ " "++show(offset)  --Removes the integer m and the stack pointer from the top of the stack and replaces it by the value at offset m from the removed pointer. 
storeO offset = "STORE_O " ++ show(offset) --Removes the stack pointer on top of the stack and the cell below and replaces the cell at offset m from the removed stack pointer by the removed cell.
storeOS= "STORE_OS"--Removes the integer m, the stack pointer and the cell below from the top of the stack and replaces the cell at offset m inter by the removed cell.
alloc num = "ALLOC_" ++ show(num) --Creates n void cells on top of the stack. To deallocate n cells from the top of the stack use a negative number. Removes the top n cells from the stack.
allocS = "ALLOC_S" --ALLOC_S: Removes the integer n from the top of the stack and creates n void cells on top of the stack. To deallocate n cells from the top of the stack use a negative number.
--loadC chr = "LOAD_C " ++ chr --Load a character onto the stack.

--Data manipulation on heap
loadH = "LOAD_H" --Removes the heap pointer on top the stack and replaces it by the elements of the heap record loaded on top of the stack.
loadHO offset = "LOAD_HO " ++ show(offset) --Removes the heap pointer on top of the stack and replaces it by the value at offset m from the removed pointer Note m cannot be negative  or utside the heap block
storeH num = "STORE_H " ++ show(num) -- Removes the top m elements of the stack and replaces them by a heap pointer to a heap record consisting of those top m elements.
storeHO offset = "STORE_HO " ++ show(offset) --Removes the heap pointer on top of the stack and the cell below and replaces the cell at offset m from the removed heap pointer by the removed cell. Note m cannot be negative nor can it be outside the heap block which is being accessed.

allocH num = "ALLOC_H " ++ show(num) -- Creates a block of n void cells on top of the heap and places the heap pointer to them on the top of the stack. Note one can only allocate to the heap there is no deallocation. Note that n must be greater than zero.

--Arithmetic operations
app op = "APP " ++ op --Removes the top one/two integer/boolean values and replaces them with the resulting of applying the operation.

-- NOT PART OF M+, keeping here just in case 
{-Float/real operations:
ADD_F SUB_F DIV_F MUL_F
arity 2
NEG_F FLOOR CIEL
arity 1-}

--Integer operations: arity 2
add_ = "ADD"
sub_ = "SUB"
div_ = "DIV"
mul_ = "MUL"
neg_ = "NEG" --
--FLOAT not part of M+

-- NOT PART OF M+
{- 
--comparison operations on floats/reals -- arity 2
LT_F,LE_F,GT_F,GE_F,EQ_F
-}

--comparison operations on integers --arity 2
lt = "LT"
le = "LE"
gt = "GT"
ge = "GE"
eq = "EQ"

--NOT PART OF M+
{-
(e) comparison operations on characters
LT_C,LE_C,GT_C,
GE_C,EQ_C
arity 2
-}

-- logical operations on booleans -- arity 2
and_ = "AND"
or_ = "0R"
not_ = "NOT" --arity 1


readI = "READ_I" --Read integer value onto top of stack.
readB = "READ_B" --Read boolean value onto top of stack.
-- readC = "READ_C" --Reads next character to top of stack (the system buffers characters until the first NOT PART OF M+ newline).
-- readF = "READ_F" --Read float/real value onto top of stack. NOT PART OF M+


printI = "PRINT_I" --Removes top integer value of stack and prints it.
printB = "PRINT_B" --Removes top boolean value of stack and prints it.
--printC = "PRINT_C"  --Removes top character value from top of stack and prints it. NOT PART OF M+
--printF = "PRINT_F" --Removes top real value of stack and prints it. NOT PART OF M+


printCode :: [String] -> String
printCode [] = []
printCode (x:xs) = x ++ "\n" ++ printCode xs

startProg :: I_prog -> String
startProg (IPROG (fbdys,num,stmts)) = ppShow(printCode code) where
    (num1, statements) = codeStmts 1 stmts
    (num2,fcns) = codeFcns num1 fbdys
    (num3,start) = progStart num2 num
    code = start ++ fcns ++ statements

chaseLink :: Int -> [String]
chaseLink 0 = []
chaseLink n = (loadO (-2)):chaseLink(n-1)

progStart :: Int ->  Int -> (Int,[String])
progStart n num = (n, [(loadR sp) , (loadR sp) , (storeR fp) , (alloc num)])


codeStmts :: Int ->  [I_stmt] -> (Int, [String])
codeStmts n [] = (n, [])
codeStmts n (x:xs) = (end, fcn++fcns) where
    (num1,fcn) = codeStmt n x
    (end,fcns) = codeStmts num1 xs


codeStmt :: Int ->  I_stmt -> (Int, [String])
codeStmt n stm = case stm of
    IASS (lev,off,exp) -> (n, ld1 ++ (loadR fp) : (chaseLink lev) ++ [(storeO off)]) where
        ld1 = codeExpr exp
    IWHILE (exp,stm) -> (n,["test"])
    ICOND (exp,stm1,stm2)-> (end, expression++jmpStm1++statement1++jmpStm2++firstLable++statement2++secondLable) where
        expression =  (codeExpr exp)
        jmpStm1 = [jumpC ("lable"++show(n))]
        jmpStm2 = [jumpC ("lable"++show(n+1))]
        firstLable = [genLable "" n]
        secondLable = [genLable "" (n+1)]
        (num2, statement1) = codeStmt (n+2) stm1
        (end, statement2) = codeStmt num2 stm2        
    IREAD_I (lev,off) -> (n, readI:(loadR fp):[storeO off])
    IPRINT_I (exp) -> (n, (codeExpr exp) ++ [printI])
    IREAD_B (lev,off) -> (n, (readI:(loadR fp):[storeO off]))
    IPRINT_B (exp) -> (n, (codeExpr exp) ++ [printB])
    IRETURN (exp) ->  (n, codeExpr exp)
    IBLOCK (fbodies, num, stmts) -> (num3,  start ++ fcns ++ statements) where
        (num1, statements) = codeStmts n stmts
        (num2, fcns)  = codeFcns num1 fbodies        
        (num3, start) = progStart num2 num 
  

{-
codeExprs :: Int ->  [I_expr] -> (Int, [String])
codeExprs [] = []
codeExprs (x:xs) -> (codeExpr x) ++ (codeExprs xs)-}

codeExpr ::  I_expr ->  [String]
codeExpr exp = case exp of 
    INUM num -> [loadI num]
    IBOOL val -> [loadB (show(val))]
    IID (lev, off) -> (loadR fp):(chaseLink lev)++[storeO off]
    IAPP (opn, exps) -> ((foldl (\acc x -> codeExpr x)[] exps)++(getOperation opn))
    
getOperation :: I_opn -> [String]
getOperation x = case x of
    ICALL (str,lev) -> (alloc 1):([loadR fp]++(chaseLink lev))++[loadR fp]++[storeR cp]++[jump str]
    IADD -> [app add_]
    IMUL -> [app mul_]
    ISUB -> [app sub_]
    IDIV -> [app div_]
    INEG -> [app neg_]
    ILT  -> [app lt]
    ILE -> [app le]
    IGT -> [app gt]
    IGE -> [app ge]
    IEQ -> [app eq]
    INOT -> [app not_]
    IAND -> [app and_]
    IOR -> [app or_]


genLable :: String -> Int -> String
genLable "" n = "lable" ++ show(n)++" :"
genLable s n = s ++ show(n) ++ " :" 

codeFcns :: Int ->  [I_fbody] -> (Int, [String])
codeFcns n [] = (n,[])
codeFcns n (x:xs) = (end, fcn++fcns) where
    (num1,fcn) = codeFcn n x
    (end,fcns) = codeFcns num1 xs

codeFcn :: Int -> I_fbody -> (Int, [String])
codeFcn n f =  (n, ["yes"])
