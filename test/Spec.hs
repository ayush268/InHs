import Control.Exception
import qualified Types
import qualified Execution as Ex
import qualified Data.Map as Map

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

    -- Identifier binding succeeds
    let p6 = Types.BindIdent s1 s2
    let p7 = Types.Var s1 p6
    let p8 = Types.Var s2 p7

    -- Identifier binding fails; Variable out of scope
    let p9 = Types.Var s1 Types.Skip
    let p10 = Types.Multiple [p9, Types.BindIdent s2 s1]
    let p11 = Types.Var s2 p10

    -- Unification fails
    let p15 = Types.BindValue s1 $ Types.Lit 5
    let p16 = Types.BindValue s2 $ Types.Lit 7
    let p17 = Types.BindIdent s1 s2
    let p12 = Types.Multiple [p15, p16, p17]
    let p13 = Types.Var s1 p12
    let p14 = Types.Var s2 p13

    -- Unification succeeds
    let p21 = Types.BindValue s1 $ Types.Lit 5
    let p22 = Types.BindValue s2 $ Types.Lit 5
    let p23 = Types.BindIdent s1 s2
    let p18 = Types.Multiple [p21, p22, p23]
    let p19 = Types.Var s1 p18
    let p20 = Types.Var s2 p19

    -- Bind record to a value
    let p24 = Types.BindValue s1 $ Types.Record 12 $ Map.fromList [(1, s2), (2, s3)]
    let p25 = Types.Var s1 p24
    let p28 = Types.Var s2 p25
    let p29 = Types.Var s3 p28

    -- Assign procedure value along with closure
    let p26 = Types.BindValue s2 $ Types.Proc ["a", "b"] Types.Skip
    let p27 = Types.Var s2 p26

    putStrLn "\n"
    let (_, x, _, _) = Ex.executeProgram p5
    print x
    let (_, x, _, _) = Ex.executeProgram p8
    print x
    let (y, _, _, _) = Ex.executeProgram p11
    print y
    let (y, _, _, _) = Ex.executeProgram p14
    print x
    let (_, x, _, _) = Ex.executeProgram p20
    print x
    let (_, x, _, _) = Ex.executeProgram p29
    print x
    let (_, x, _, _) = Ex.executeProgram p27
    print x
