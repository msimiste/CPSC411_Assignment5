
module ST where

import AST

--insertion

data SYM_DESC = ARGUMENT (String,M_type,Int) --(name, type, dimensions)
              | VARIABLE (String,M_type,Int)  --(name, type, dimensions)
              | FUNCTION (String,[(M_type,Int)],M_type) --(name,[(type(s),dimension(s))],returnType)
              | DATATYPE String --(name of the datatype)
              | CONSTRUCTOR (String, [M_type], String) -- (name,[type], name of the datatype)
              deriving (Eq,Show,Ord, Read)

data SYM_I_DESC = I_VARIABLE (Int,Int,M_type,Int) --(level,offset,type,dimension)
              | I_FUNCTION (Int,String,[(M_type,Int)],M_type) --(level,label,[(argType,dimension)]type)
              | I_CONSTRUCTOR (Int,[M_type],String) --(constructor#,[type],datatypename)
              | I_TYPE [String]
              deriving (Eq,Show,Ord, Read)
              
data ScopeType = L_PROG 
              | L_FUN M_type -- M_type = return type
              | L_BLK 
              | L_CASE
              deriving (Eq,Show,Ord, Read)
              
data SYM_VALUE = Var_attr (Int,M_type,Int)
              | Fun_attr (String,[(M_type,Int)],M_type)
              | Con_attr (Int, [M_type], String)
              | Typ_attr [String]
              deriving (Eq,Show,Ord, Read)
                


data SYM_TABLE = Symbol_table (ScopeType,Int,Int,[(String,SYM_VALUE)]) --(Scopetype,#locVars,#args,[(Name,SymValue)])
    deriving (Eq,Show,Ord, Read)

type ST = [SYM_TABLE]

