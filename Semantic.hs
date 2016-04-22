module Semantic where

import AST
import SymbolTable as S
import IRDataType
import ST
import Text.Show.Pretty

--Starting function 
typeProg :: ST -> AST -> Bool
typeProg st (M_prog (decls,stmts)) = truthVal where
    truthVal = truth1 && truth2 --error("truth1 : "++show(truth1) ++ ("truth2 : ")++show(truth2)) truth1 && truth2
    truth1 = checkDecls st decls --error("showing decls :" ++ show(decls)) 
    truth2 = checkStmts st stmts --error("showing stmts :" ++ show(stmts)) 
    


checkDecls :: ST -> [M_decl] -> Bool
checkDecls st [] = True
checkDecls st (x:xs) = this && that where
    this = (checkDecl st x)
    that = (checkDecls st xs) --error("line 22: " ++ show(xs) ) 


checkDecl :: ST -> M_decl -> Bool
checkDecl st dec = case dec of
    M_var(str,listExp,typ) -> checkSameMexpr st listExp typ && truthVal1 where
        truthVal1 = case (S.look_up st str) of
            I_VARIABLE a  -> True --error("line 24 :" ++ show(a))
            x -> error("Semantic error checkDecl_M_var " ++ show(x))
    M_fun(name,listPars,retTyp,decs,stmts) -> truthVal where
        truth1 = checkDecls st decs
        truth2 = checkStmts st stmts
        truth3 =  case (S.look_up st name) of 
            I_FUNCTION(_,_,funPars,_) -> compParams listPars funPars
            x -> error ("Semantic error checkDecl_M_fun "++ show(x))
        truthVal = truth1 && truth2 && truth3
  
  
checkStmts :: ST -> [M_stmt] -> Bool
checkStmts st [] = True
checkStmts st (x:xs) = (checkStmt st x) && (checkStmts st xs)

    
checkStmt :: ST -> M_stmt -> Bool
checkStmt st stmt = case stmt of
    M_ass (str,expList,exp) -> truth1 where --error("reached line 41: "++show(stmt))
        truth1 = case (S.look_up st str) of
            I_VARIABLE(_,_,typ,_) ->  checkSameMexpr st (exp:expList) typ --error("reached line 44: " ++ show(checkSameMexpr [exp] typ))--checkSameMexpr [exp] typ --error("reached line 44: " ++ show(checkSameMexpr [exp] typ))
            I_FUNCTION(_,_,_,typ) -> checkSameMexpr st (exp:expList) typ--  error("just a 2nd test :" ++ show(checkSameMexpr st [exp] typ))
            x -> error("showing :" ++ show(x))
    M_while (exp,stmt) -> (checkExpr st exp) && (checkStmt st stmt) --error("Reached line 46: ")
    M_cond (exp,stmt1,stmt2) -> (checkStmt st stmt1) && (checkStmt st stmt2)--error("Reached line 47:")--((checkExpr st  exp) && (checkStmt st stmt1) && (checkStmt st stmt2))--("Reached line 47:")
    M_read (str,exprs) -> truth1 where
        truth1 = case(S.look_up st str) of
            I_VARIABLE(_,_,typ,_) -> (checkSameMexpr st exprs typ)--error("line 51 : "++show( checkSameMexpr exprs typ)) --error("Reached line 50:" ++ show (stmt))
            x -> error("error line 58: " ++ show(x))
    M_print expr -> checkExpr st expr --error("line 52 : " ++ show(checkExpr st expr)++"\n stmt: "++show(stmt)++" \nexpr: "++show(expr)) --error("Reached line 51: "++show(stmt))
    M_return expr -> checkExpr st expr -- error("Reached line 52:") 
    M_block (decs,stmts) -> (checkDecls st decs) && (checkStmts st stmts) -- error("Reached line 54:") 
    
           
checkExpr :: ST -> M_expr -> Bool
checkExpr st exp = case exp of
    M_ival x -> checkMival exp
    M_rval x -> checkMrval exp
    M_bval x -> checkMbval exp
    M_size x -> True
    M_id (str,exp) -> foldl(\truth x -> checkMival x) True exp
    M_app (operation,exps) -> validateOperation st operation exps


validateOperation :: ST -> M_operation -> [M_expr] -> Bool
validateOperation st operate exprs = case operate of
    M_fn str ->  case (foldl(\truth x -> checkExpr st x)True exprs) of
        True -> case(S.look_up st str) of
            I_FUNCTION(_,_,list,typ) -> case (foldl(\truth x -> x == typ)True (condenseList(exprToType st exprs))) of --error("line 84 : "++ppShow(S.look_up st str))
				True -> True
				False -> error("line 80: params: "++ ppShow((convertParams list)==(exprToType st exprs))++"\n :"++ppShow(operate)++"\n list: "++ppShow(list)++"\n exprs: "++ppShow(exprs)) -- error("error in M_fn of validateOperation : " ++ show(exprToType st exprs)++ " not equal to \n"++show( list))-- error("line 71 : "++"\n exprs : "++ show(exprs)++"\n totype: " ++show(exprToType st exprs)++"\n list: "++ show(list) ++ "\n totype: "++show(convertParams list))  x -> error("test: " )
            _ -> error("test: ")
        False -> error("line 84 :")
    M_add -> case (foldl(\truth x -> x `elem` [M_int,M_real]) True (exprToType st exprs)) of
        True -> True
        False -> error ("error in M_add of validateOperation : " ++ show(exprs))
    M_mul -> case (foldl(\truth x -> x `elem` [M_int,M_real]) True (exprToType st exprs)) of
        True -> True
        False -> error ("error in M_mul of validateOperation : " ++ show(exprs))
    M_sub -> case (foldl(\truth x -> x `elem` [M_int,M_real]) True (exprToType st exprs)) of
        True -> True
        False -> error ("error in M_sub of validateOperation : " ++ show(exprs))
    M_div -> case (foldl(\truth x -> x `elem` [M_int,M_real]) True (exprToType st exprs)) of
        True -> True
        False -> error ("error in M_div of validateOperation : " ++ show(exprs))
    M_neg -> case (foldl(\truth x -> x `elem` [M_int,M_real]) True (exprToType st exprs)) of
        True -> True
        False -> error ("error in M_neg of validateOperation : " ++ show(exprs)) 
    M_lt -> case (foldl(\truth x -> x `elem` [M_int,M_real]) True (exprToType st exprs)) of
        True -> True
        False -> error ("error in M_lt of validateOperation : " ++ show(exprs))
    M_le -> case (foldl(\truth x -> x `elem` [M_int,M_real]) True (exprToType st exprs)) of
        True -> True
        False -> error ("error in M_le of validateOperation : " ++ show(exprs))
    M_gt -> case (foldl(\truth x -> x `elem` [M_int,M_real]) True (exprToType st exprs)) of
        True -> True
        False -> error ("error in M_gt of validateOperation : " ++ show(exprs))
    M_ge -> case (foldl(\truth x -> x `elem` [M_int,M_real]) True (exprToType st exprs)) of
        True -> True
        False -> error ("error in M_ge of validateOperation : " ++ show(exprs))
    M_eq -> case (foldl(\truth x -> x `elem` [M_int,M_real]) True (exprToType st exprs)) of
        True -> True
        False -> error ("error in M_eq of validateOperation : " ++ show(exprs))
    M_not -> case (foldl(\truth x -> x == M_bool) True (exprToType st exprs)) of
        True -> True
        False -> error ("error in M_not of validateOperation : " ++ show(exprs))
    M_and -> case (foldl(\truth x -> x == M_bool) True (exprToType st exprs)) of
        True -> True
        False -> error ("error in M_and of validateOperation : " ++ show(exprs))
    M_or -> case (foldl(\truth x -> x == M_bool) True (exprToType st exprs)) of
        True -> True
        False -> error ("error in M_or of validateOperation : " ++ show(exprs))
    _ -> error("semantic line 128: ")
            
exprToType :: ST -> [M_expr] -> [M_type]
exprToType st [] = []
exprToType st (x:xs) = case x of
    M_ival _ ->  M_int:(exprToType st xs)
    M_bval _ -> M_bool:(exprToType st xs)
    M_rval _ -> M_real:(exprToType st xs)
    M_id (str,exps) -> (getM_id st str):(exprToType st xs)
    M_app (op,exps) -> (exprToType st exps) ++ (exprToType st xs)
 


getM_id :: ST -> String -> M_type
getM_id st str = case (S.look_up st str) of
    I_VARIABLE(_,_,typ,_) -> typ
    I_FUNCTION(_,_,_,typ) -> typ
    x -> error("error in getM_id : "++show(x))
    
convertParams :: [(M_type,Int)] -> [M_type]
convertParams [] = []
convertParams ((x,t):xs) = (x:(convertParams xs)) 


condenseList :: [M_type] -> [M_type]
condenseList [] = []
condenseList(x:y:[]) 
	| x == y = [y]
	| otherwise = (x:[y])
condenseList (x:y:xs)
    | x == y = condenseList (y:xs)
    | otherwise = x:(condenseList (y:xs))
    
checkSameMexpr :: ST -> [M_expr] -> M_type -> Bool
checkSameMexpr st [] typ = True --error("checkSameMexpr line 101 : "++show(typ))
checkSameMexpr st exp typ = case (foldl(\truth x -> x == typ) True (exprToType st exp)) of
    True -> True
    False -> error("error in M_int of checkSameMexpr: \n" ++ show(exprToType st exp) ++ "\n "++show(typ) ++" are not equal\n"++ ppShow (st))

checkMival :: M_expr -> Bool
checkMival x = case x of
    M_ival x -> True
    _ -> False

checkMbval :: M_expr -> Bool
checkMbval x = case x of
    M_bval x -> True
    _ -> False

checkMrval :: M_expr -> Bool
checkMrval x = case x of
    M_rval x -> True
    _ -> False

compareLists :: [M_type] -> [M_type] -> Bool
compareLists [] [] = True --error("line 168: " ++ppShow(b))--True
compareLists  x [] = False
compareLists  [] x = False
compareLists  (x:xs) (y:ys) = error("line 171 : "++ppShow(x:xs== y:ys)++(ppShow(xs==ys))++ "\n x: "++ppShow(xs)++"\n y:"++ppShow(ys))--(x == y )&&(compareLists xs ys)--error("line 169: exprs: " ++ppShow((x:xs))++" params: "++ppShow(b))--( (x `elem` b) && (compareLists xs b)


compParams :: [(String,Int,M_type)] -> [(M_type,Int)] -> Bool
compParams [] [] = True
compParams _ [] = False
compParams [] _ = False
compParams  ((_,num1,typ1):ls1) ((typ2,num2):ls2) = (num1 == num2) && (typ1 == typ2) && (compParams ls1 ls2)
