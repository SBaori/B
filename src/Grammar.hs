module Grammar where

type Varname = String
type FuncName = String

data Node a = Node {
    nodeId :: a,
    nodePos :: (Int, Int)
}
    deriving (Show)

data Program = Program GlobalUnits

data ScopedUnits = ScopedUnit ScopedUnit ScopedUnits
                | ScopedUnitsEnd
    deriving (Show)

data GlobalUnits = GlobalUnitVarDeclr GlobalVarDeclrs GlobalUnits
                | GlobalUnitFuncDfn FuncDfn GlobalUnits
                | GlobalUnitsEnd
    deriving (Show)

-- @todo: multi-dimensional global arrays.
data GlobalVarDeclrs = GlobalVarDeclrs (Node String) Expr
                    | GlobalVarDeclrsArr (Node String) Expr [Expr]
    deriving (Show)

data ScopedUnit = ScopedUnitExpr Expr
                | ScopedUnitVarDeclr ScopedVarDeclrs
                | ScopedUnitExternDeclr ExternVarDeclrs
                | ScopedUnitControl Control
                | ScopedUnitReturn (Node String) (Maybe Expr)
                | ScopedUnitContinue (Node String)
                | ScopedUnitBreak (Node String)
    deriving (Show)

-- @todo: multi-dimensional auto arrays.
data ScopedVarDeclrs = ScopedVarDeclrsEnd (Node String) (Maybe Expr)
                    | ScopedVarDeclrs (Node String) (Maybe Expr) ScopedVarDeclrs
                    | ScopedVarDeclrsArrEnd (Node String) Expr (Maybe [Expr])
                    | ScopedVarDeclrsArr (Node String) Expr (Maybe [Expr]) ScopedVarDeclrs
    deriving (Show)

data ExternVarDeclrs = ExternVarDeclrsEnd (Node String)
                    | ExternVarDeclrs (Node String) ExternVarDeclrs
    deriving(Show)

data Control = ControlIf IfElse
            | ControlLoop Loop
    deriving (Show)

data Loop = WhileScoped Expr ScopedUnits
        | WhileUnscoped Expr ScopedUnit
    deriving (Show)

data If = IfScoped Expr ScopedUnits
        | IfUnScoped Expr ScopedUnit
    deriving(Show)

data IfElse = IfElse If ElseIf
    deriving (Show)

data ElseIf = ElseIf If ElseIf
            | ElseScoped ScopedUnits
            | ElseUnScoped ScopedUnit
            | Fi
    deriving(Show)

data FuncDfn = FuncDfn (Node FuncName) [(Node String)] ScopedUnits
    deriving (Show)

-- R -> L
data Expr = Ternary' Ternary
            | Assign Ternary Expr
            | AssignAdd Ternary Expr
            | AssignSub Ternary Expr
            | AssignMul Ternary Expr
            | AssignDiv Ternary Expr
            | AssignBitLShift Ternary Expr
            | AssignBitRShift Ternary Expr
            | AssignMod Ternary Expr
            | AssignBitAnd Ternary Expr
            | AssignBitOr Ternary Expr
            | AssignBitXor Ternary Expr
    deriving (Show)

data Ternary = LogicalOr LogicalOr
            | Ternary LogicalOr LogicalOr Ternary
    deriving (Show)

-- L -> R
data LogicalOr = LogicalAnd LogicalAnd
                | Or LogicalOr LogicalAnd 
    deriving (Show)

data LogicalAnd = BitOr' BitOr
                | And LogicalAnd BitOr
    deriving (Show)

data BitOr = BitXor' BitXor
            | BitOr BitOr BitXor
    deriving (Show)

data BitXor = BitAnd' BitAnd
            | BitXor BitXor BitAnd
    deriving (Show)

data BitAnd = EqualityExpr EqualityExpr
            | BitAnd BitAnd EqualityExpr
    deriving (Show)

data EqualityExpr = RelationExpr RelationExpr
                    | Equal EqualityExpr RelationExpr
                    | NotEqual EqualityExpr RelationExpr
    deriving (Show)

data RelationExpr = BitShifts BitShifts
                    | Less RelationExpr BitShifts
                    | ULess RelationExpr BitShifts
                    | LessEq RelationExpr BitShifts
                    | ULessEq RelationExpr BitShifts
                    | Greater RelationExpr BitShifts
                    | UGreater RelationExpr BitShifts
                    | GreaterEq RelationExpr BitShifts
                    | UGreaterEq RelationExpr BitShifts
    deriving (Show)

data BitShifts = AddSub AddSub
                | LeftShift BitShifts AddSub
                | RightShift BitShifts AddSub
    deriving (Show)

data AddSub = MulDivMod MulDivMod
            | Add AddSub  MulDivMod
            | Sub AddSub MulDivMod
    deriving (Show)

data MulDivMod = PreFix PreFix
                | Mul MulDivMod PreFix
                | Div MulDivMod PreFix
                | Mod MulDivMod PreFix
    deriving (Show)

-- R -> L
data PreFix = PostFix PostFix
            | PreIncr PreFix
            | PreDecr PreFix
            | Negate PreFix
            | DeRef PreFix
            | Addr PreFix
            | UnarySub PreFix
    deriving (Show)

-- L -> R
data PostFix = Element Element
            | Opr Element Opr
    deriving (Show)

data Opr = FuncCallEnd [Expr]
        | FuncCall [Expr] Opr
        | OffsetEnd Expr
        | Offset Expr Opr
        | PostIncrEnd
        | PostIncr Opr
        | PostDecrEnd
        | PostDecr Opr
    deriving (Show)

data Element = Num (Node String)
            | Var (Node Varname)
            | Str (Node String)
            | Expr Expr
    deriving (Show)