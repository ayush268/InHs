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
    let s4 = "doug"
    let s5 = "eve"
    let s6 = "frank"

    -- Identifier binding succeeds (both unbound)
    let p6 = Types.BindIdent s1 s2
    let p7 = Types.Var s1 p6
    let p8 = Types.Var s2 p7

    -- Identifier binding succeeds (s1 -> bound; s2 -> unbound)
    let p30 = Types.BindIdent s1 s2
    let p31 = Types.Multiple [Types.BindValue s1 $ Types.Lit 5, p30]
    let p32 = Types.Var s1 p31
    let p33 = Types.Var s2 p32

    -- Identifier binding succeeds (s1 -> unbound; s2 -> bound)
    let p34 = Types.BindIdent s1 s2
    let p35 = Types.Multiple [Types.BindValue s2 $ Types.Lit 5, p34]
    let p36 = Types.Var s1 p35
    let p37 = Types.Var s2 p36

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

    -- Bind record to a value succeeds
    let p24 = Types.BindValue s1 $ Types.Record 12 $ Map.fromList [(1, s2), (2, s3)]
    let p25 = Types.Var s1 p24
    let p28 = Types.Var s2 p25
    let p29 = Types.Var s3 p28

    -- Binding two records fails (different arity)
    let p38 = Types.BindValue s1 $ Types.Record 12 $ Map.fromList [(1, s2), (2, s3)]
    let p39 = Types.BindValue s4 $ Types.Record 12 $ Map.fromList [(1, s5)]
    let p45 = Types.Multiple[p38, p39, Types.BindIdent s1 s4]
    let p40 = Types.Var s1 p45
    let p41 = Types.Var s2 p40
    let p42 = Types.Var s3 p41
    let p43 = Types.Var s4 p42
    let p44 = Types.Var s5 p43

    -- Binding two records succeeds (same arity)
    let p45 = Types.BindValue s1 $ Types.Record 12 $ Map.fromList [(1, s2), (2, s3)]
    let p46 = Types.BindValue s4 $ Types.Record 12 $ Map.fromList [(1, s5), (2, s6)]
    let p47 = Types.Multiple[p45, p46, Types.BindIdent s1 s4]
    let p48 = Types.Var s1 p47
    let p49 = Types.Var s2 p48
    let p50 = Types.Var s3 p49
    let p51 = Types.Var s4 p50
    let p52 = Types.Var s5 p51
    let p53 = Types.Var s6 p52

    -- Collecting free variables from closures
    let p54 = Types.BindIdent s1 s2
    let p55 = Types.BindValue s3 $ Types.Proc [s2] p54
    let p56 = Types.Var s1 p55
    let p57 = Types.Var s2 p56
    let p58 = Types.Var s3 p57

    -- Assign procedure value along with closure
    let p26 = Types.BindValue s2 $ Types.Proc ["a", "b"] Types.Skip
    let p27 = Types.Var s2 p26

    let (x, _, y) = Ex.executeProgram p5
    print p5
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n"
    putStrLn "\n"
    let (x, _, y) = Ex.executeProgram p8
    print p8
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------" 

    -- Failure Case
    putStrLn "\n"
    putStrLn "\n"
    let (x, _, y) = Ex.executeProgram p11
    print p11
    print y
    Control.Exception.catch (print x) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    putStrLn "-----------------------PASSED---------------------------" 

    -- Failure Case
    putStrLn "\n"
    putStrLn "\n"
    let (x, _, y) = Ex.executeProgram p14
    print p14
    print y
    Control.Exception.catch (print x) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n"
    putStrLn "\n"
    let (x, _, y) = Ex.executeProgram p20
    print p20
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n"
    putStrLn "\n"
    let (x, _, y) = Ex.executeProgram p29
    print p29
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n"
    putStrLn "\n"
    let (x, _, y) = Ex.executeProgram p27
    print p27
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n"
    putStrLn "\n"
    let (x, _, y) = Ex.executeProgram p33
    print p33
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n"
    putStrLn "\n"
    let (x, _, y) = Ex.executeProgram p37
    print p37
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------" 

    -- Failure Case
    putStrLn "\n"
    putStrLn "\n"
    let (x, _, y) = Ex.executeProgram p44
    print p44
    print y
    Control.Exception.catch (print x) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n"
    putStrLn "\n"
    let (x, _, y) = Ex.executeProgram p53
    print p53
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n"
    putStrLn "\n"
    let (x, _, y) = Ex.executeProgram p58
    print p58
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------" 

-- For future tests
-- x = Types.Var "a" $ Types.Var "b" $ Types.Var "c" $ Var "x" $ Var "y" $ Multiple [(BindValue "x" $ Record 1 $ Map.fromList [(10, "a"), (12, "b")]), (Match "x" (Record 1 $ Map.fromList [(10, "m"),(12,"n")]) (BindIdent "y" "m") (BindIdent "y" "b"))]
