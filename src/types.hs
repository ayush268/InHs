-- Module listing out types for the interpreter

--module types(Statement(..)) where

type Identifier = String
type Literal = Int

data Statement = Skip
                 | Multiple {stmts :: [Statement]}
                 | Var {ident :: Identifier,
                        stmt  :: Statement}
                 | Bind {ident   :: Identifier,
                         literal :: Literal}
                 | Record {label :: Literal,
                           pairs :: [(Literal, Identifier)]}
                 | Proc {parameters :: [Identifier],
                         stmt       :: Statement}
                 | Conditional {ident   :: Identifier,
                                fststmt :: Statement,
                                sndstmt :: Statement}
                 | Match {ident   :: Identifier,
                          fststmt :: Statement,
                          sndstmt :: Statement}
                 | Apply {func       :: Identifier,
                          parameters :: [Identifier]} deriving (Show)

-- instance Read Statement where
