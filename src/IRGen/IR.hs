module IRGen.IR where
import Control.Monad.State (StateT(..))
import Error
import qualified Data.Map.Strict as M
import Grammar

type IR = StateT SemState (Either Error)

-- @todo: map the variable name directly to its mangled name (Int,Int) -> [name', (Int,Int)]
type Scopes = [Scope]

data ScopeType = Func
                | Loop String String
                | Branch
                | Anon
    deriving (Show)

data Scope = Scope {
    scopeType :: ScopeType,
    scopeDepth :: Int,
    autoSymbols :: M.Map String (Int, Int),
    externSymbols :: M.Map String (Int, Int)
}
    deriving (Show)

-- @todo: Add warnings and error accumulation.
data SemState = SemState {
    ir :: [String],
    irBuffer :: [String],
    globalSymbols :: M.Map String (Int, Int),
    regNum :: !Integer,
    labelNum :: !Integer,
    uniqCounter :: !Integer
}
    deriving (Show)

-- @todo: Add a new type for addresses.
data ExprRes = Temporary {name :: Node String}
            | Storage {name :: Node String}
            | Literal {num :: Node Word}
    deriving (Show)

runIRGen :: IR a -> SemState -> Either Error (a, SemState)
runIRGen = runStateT