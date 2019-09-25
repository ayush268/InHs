import Control.Exception
import qualified Types
import qualified Execution as Ex

main :: IO ()
main = do
    let p1 = Types.Var "x" Types.Skip
    let p2 = Types.Var "y" p1
    let p3 = Types.Var "z" p2
    let p4 = Types.Multiple [Types.Skip, p1, p2]
    let p5 = Types.Multiple [p4, p3, Types.Multiple [p1, p2, p3]]

    let s1 = "alice"
    let s2 = "bob"
    let s3 = "charles"

    -- Unification succeeds
    let p6 = Types.BindIdent s1 s2
    let p7 = Types.BindValue s1 (Types.Lit 5)
    let p8 = Types.BindValue s2 (Types.Lit 5)
    let p9 = Types.Multiple [p6, p7, p8]

    -- Unification fails
    let p10 = Types.BindIdent s1 s3
    let p11 = Types.BindValue s3 (Types.Lit 5)
    let p12 = Types.Multiple [p7, p10, p11]

    -- Variable out of scope
    let p13 = Types.Multiple [Types.Var s2 Types.Skip, Types.BindIdent s2 s1]
    let p14 = Types.Var s1 p13

    putStrLn "\n"
    let (_, x, _) = Ex.executeProgram p3
    print x
    let (_, x, _) = Ex.executeProgram p4
    print x
    let (_, x, _) = Ex.executeProgram p5
    print x
    let (y, _, _) = Ex.executeProgram p9
    print y
    let (y, _, _) = Ex.executeProgram p12
    print y
    let (y, x, _) = Ex.executeProgram p14
    print y
