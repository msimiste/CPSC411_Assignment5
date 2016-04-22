
module MikeAM where

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
jump0 offset = "JUMP_O " ++ offest -- Removes the (positive) integer on top of the stack and moves the code pointer that number of steps ahead: 1= next instruction 2 = skip instruction 3 = skip 2 instructions
halt = "HALT" -- End of execution


-- Data manipulation on stack:
--loadf val = "LOAD_F" ++ show(val) -- Load a constant float/real number onto the stack.
loadI num = "LOAD_I " ++ show(num) -- Load a constant integer onto the stack.
loadB val = "LOAD_B " ++ val --load a constant boolean onto the stack.
loadC chr = "LOAD_C " ++ chr --Load a character onto the sack.
loadO offset = "LOAD_O " ++ show(offest) --Removes the stack pointer on top of the stack and replaces it by the value at offset m from the removed pointer. Note m can be negative 
loadOS num offset = "LOAD_OS " ++ show(num) ++ " "++show(offset)  --Removes the integer m and the stack pointer from the top of the stack and replaces it by the value at offset m from the removed pointer. 
store0 offset = "STORE_O " ++ show(offest) --Removes the stack pointer on top of the stack and the cell below and replaces the cell at offset m from the removed stack pointer by the removed cell.
storeOS= "STORE_OS " ++ show(num) ++ " " ++ show(offset) --Removes the integer m, the stack pointer and the cell below from the top of the stack and replaces the cell at offset m inter by the removed cell.
alloc num = "ALLOC_" ++ show(num) --Creates n void cells on top of the stack. To deallocate n cells from the top of the stack use a negative number. Removes the top n cells from the stack.
allocS = "ALLOC_S" --ALLOC_S: Removes the integer n from the top of the stack and creates n void cells on top of the stack. To deallocate n cells from the top of the stack use a negative number.

--Data manipulation on heap
loadH = "LOAD_H" --Removes the heap pointer on top the stack and replaces it by the elements of the heap record loaded on top of the stack.
loadHO offest = "LOAD_HO " ++ show(offset) --Removes the heap pointer on top of the stack and replaces it by the value at offset m from the removed pointer Note m cannot be negative  or utside the heap block
storeH num = "STORE_H " ++ show(num) -- Removes the top m elements of the stack and replaces them by a heap pointer to a heap record consisting of those top m elements.
storeHO offset = "STORE_HO " ++ show(offset) --Removes the heap pointer on top of the stack and the cell below and replaces the cell at offset m from the removed heap pointer by the removed cell. Note m cannot be negative nor can it be outside the heap block which is being accessed.

allocH num = "ALLOC_H " show(num) -- Creates a block of n void cells on top of the heap and places the heap pointer to them on the top of the stack. Note one can only allocate to the heap there is no deallocation. Note that n must be greater than zero.

--Arithmetic operations
app op = "APP " ++ op --Removes the top one/two integer/boolean values and replaces them with the resulting of applying the operation.

-- NOT PART OF M+, keeping here just in case 
{-Float/real operations:
ADD_F SUB_F DIV_F MUL_F
arity 2
NEG_F FLOOR CIEL
arity 1-}

--Integer operations: arity 2
add = "ADD"
sub = "SUB"
div = "DIV"
mul = "MUL"
neg = "NEG" --
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
and = "AND"
or = "0R"
not = "NOT" --arity 1

-- readF = "READ_F" --Read float/real value onto top of stack. NOT PART OF M+
readI = "READ_I" --Read integer value onto top of stack.
readB = "READ_B" --Read boolean value onto top of stack.

-- readC = "READ_C" --Reads next character to top of stack (the system buffers characters until the first NOT PART OF M+ newline).

--printF = "PRINT_F" --Removes top real value of stack and prints it. NOT PART OF M+
printI = "PRINT_I" --Removes top integer value of stack and prints it.
printB = "PRINT_B" --Removes top boolean value of stack and prints it.
--printC = "PRINT_C"  --Removes top character value from top of stack and prints it. NOT PART OF M+

printCode [String] -> String
printCode [] = []
printCode (x:xs) = x + "\n" ++ printCode xs

startProg :: I_Prog -> String
startProg (fbdys,num,stmts) = printCode code where
	fcns = codeFcns
	statements = codeStmts
	start = progStart
	code = start ++ fcns ++ statements


