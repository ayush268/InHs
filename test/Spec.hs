import Control.Exception
import qualified Types
import qualified ExecuteProgram as Ex
import qualified Data.Map as Map

main :: IO ()
main = do
    let s1 = "alice"
    let s2 = "bob"
    let s3 = "charles"
    let s4 = "doug"
    let s5 = "eve"
    let s6 = "frank"

    -- ################################# POSITIVE CASES #################################
    --
    -- TestCase 1
    -- Variable declarations (different Combinations)
    let p1 = Types.Var "x" Types.Skip
    let p2 = Types.Var "y" p1
    let p3 = Types.Var "z" p2
    let p4 = Types.Multiple [Types.Skip, p1, p2]
    let p5 = Types.Multiple [p4, p3, Types.Multiple [p1, p2, p3]]

    -- TestCase 2
    -- Identifier binding succeeds (both unbound)
    let p6 = Types.BindIdent s1 s2
    let p7 = Types.Var s1 p6
    let p8 = Types.Var s2 p7

    -- TestCase 3
    -- Identifier binding succeeds (s1 -> bound; s2 -> unbound)
    let p9 = Types.BindIdent s1 s2
    let p10 = Types.BindValue s1 $ Types.Expr $ Types.Lit 5
    let p11 = Types.Multiple [p10, p9]
    let p12 = Types.Var s1 p11
    let p13 = Types.Var s2 p12

    -- TestCase 4
    -- Unification of 3 variables
    let p14 = Types.BindIdent s3 s2
    let p15 = Types.BindIdent s2 s1
    let p16 = Types.Multiple [p15, p14]
    let p17 = Types.Var s3 p16
    let p18 = Types.Var s2 p17
    let p19 = Types.Var s1 p18

    -- TestCase 5
    -- Unification succeeds (Bound to equal values)
    let p20 = Types.BindValue s1 $ Types.Expr $ Types.Lit 5
    let p21 = Types.BindValue s2 $ Types.Expr $ Types.Lit 5
    let p22 = Types.BindIdent s1 s2
    let p23 = Types.Multiple [p20, p21, p22]
    let p24 = Types.Var s1 p23
    let p25 = Types.Var s2 p24

    -- TestCase 6
    -- Bind record to a value succeeds
    let p26 = Types.BindValue s1 $ Types.Record 12 $ Map.fromList [(1, s2), (2, s3)]
    let p27 = Types.Var s1 p26
    let p28 = Types.Var s2 p27
    let p29 = Types.Var s3 p28

    -- TestCase 7
    -- Assign procedure value along with closure
    let p30 = Types.BindValue s2 $ Types.Proc ["a", "b"] Types.Skip
    let p31 = Types.Var s2 p30

    -- TestCase 8
    -- Collecting free variables from closures
    let p32 = Types.BindIdent s1 s2
    let p33 = Types.BindValue s3 $ Types.Proc [s2] p32
    let p34 = Types.Var s1 p33
    let p35 = Types.Var s2 p34
    let p36 = Types.Var s3 p35

    -- TestCase 9
    -- Binding two records succeeds (same arity)
    let p37 = Types.BindValue s1 $ Types.Record 12 $ Map.fromList [(1, s2), (2, s3)]
    let p38 = Types.BindValue s4 $ Types.Record 12 $ Map.fromList [(1, s5), (2, s6)]
    let p39 = Types.Multiple[p37, p38, Types.BindIdent s1 s4]
    let p40 = Types.Var s1 p39
    let p41 = Types.Var s2 p40
    let p42 = Types.Var s3 p41
    let p43 = Types.Var s4 p42
    let p44 = Types.Var s5 p43
    let p45 = Types.Var s6 p44

    -- TestCase 10
    -- Conditional Statements (Taking True branch)
    -- Unification should take place
    let p46 = Types.BindIdent s1 s2
    let p47 = Types.BindValue s2 $ Types.Expr $ Types.Lit 100
    let p48 = Types.BindValue s1 $ Types.Expr $ Types.Lit 1
    let p49 = Types.Conditional s1 p46 p47
    let p50 = Types.Multiple[p48, p49]
    let p51 = Types.Var s1 p50
    let p52 = Types.Var s2 p51

    -- TestCase 11
    -- Conditional Statements (Taking False branch)
    -- Unification should NOT take place, s2 is assigned value 100
    let p53 = Types.BindIdent s1 s2
    let p54 = Types.BindValue s2 $ Types.Expr $ Types.Lit 100
    let p55 = Types.BindValue s1 $ Types.Expr $ Types.Lit 0
    let p56 = Types.Conditional s1 p53 p54
    let p57 = Types.Multiple[p55, p56]
    let p58 = Types.Var s1 p57
    let p59 = Types.Var s2 p58

    -- TestCase 12
    -- Match (Case) statement (Pattern matched)
    -- Unification of s2 and s4 should occur
    let p60 = Types.BindIdent s2 s3
    let p61 = Types.BindIdent s2 "m"
    let p62 = Types.BindValue s1 $ Types.Record 15 $ Map.fromList [(10, s4), (12, s3)]
    let p63 = Types.Match s1 (Types.Record 15 $ Map.fromList [(10, "m"),(12,"n")]) p61 p60
    let p64 = Types.Multiple[p62, p63]
    let p65 = Types.Var s1 p64
    let p66 = Types.Var s2 p65
    let p67 = Types.Var s3 p66
    let p68 = Types.Var s4 p67

    -- TestCase 13
    -- Match (Case) statement (Pattern UNmatched)
    -- Unification of s2 and s3 should occur
    let p69 = Types.BindIdent s2 s3
    let p70 = Types.BindIdent s2 "m"
    let p71 = Types.BindValue s1 $ Types.Record 15 $ Map.fromList [(10, s4), (20, s3)]
    let p72 = Types.Match s1 (Types.Record 15 $ Map.fromList [(10, "m"),(12,"n")]) p70 p69
    let p73 = Types.Multiple [p71, p72]
    let p74 = Types.Var s1 p73
    let p75 = Types.Var s2 p74
    let p76 = Types.Var s3 p75
    let p77 = Types.Var s4 p76

    -- TestCase 14
    -- Procedure Call (Without free variables)
    -- Unification of s1 and s2 should occur (arguments equated)
    let p78 = Types.BindValue s2 $ Types.Expr $ Types.Lit 1
    let p79 = Types.BindIdent "y" "x"
    let p80 = Types.BindValue "F" $ Types.Proc ["x", "y"] p79
    let p81 = Types.Apply "F" [s2, s1]
    let p82 = Types.Multiple [p80, p78, p81]
    let p83 = Types.Var s2 p82
    let p84 = Types.Var s1 p83
    let p85 = Types.Var "F" p84

    -- TestCase 15
    -- Procedure Call (With free variables)
    -- Unification of s1, s2, s3 (with 10) should occur
    let p86 = Types.BindValue s3 $ Types.Expr $ Types.Lit 10
    let p87 = Types.BindIdent "y" "x"
    let p88 = Types.BindIdent s3 "y"
    let p89 = Types.Multiple [p87, p88]
    let p90 = Types.BindValue "F" $ Types.Proc ["x", "y"] p89
    let p91 = Types.Apply "F" [s1, s2]
    let p92 = Types.Multiple [p90, p86]
    let p93 = Types.Var s3 p92
    let p94 = Types.Multiple [p93, p91]
    let p95 = Types.Var s2 p94
    let p96 = Types.Var s1 p95
    let p97 = Types.Var "F" p96

    -- TestCase 16
    -- Evaluating Expression (Addition)
    let p98 = Types.BindValue s1 $ Types.Expr $ Types.Lit 10
    let p99 = Types.BindValue s2 $ Types.Expr $ Types.Exp Types.Add (Types.Variable s1) (Types.Lit 20)
    let p100 = Types.Multiple [p98, p99]
    let p101 = Types.Var s2 p100
    let p102 = Types.Var s1 p101

    -- TestCase 17
    -- Evaluating Expression (Multiplication)
    let p103 = Types.BindValue s1 $ Types.Expr $ Types.Lit 12
    let p104 = Types.BindValue s2 $ Types.Expr $ Types.Lit 27
    let p105 = Types.BindValue s3 $ Types.Expr $ Types.Exp Types.Mult (Types.Variable s1) (Types.Variable s2)
    let p106 = Types.Multiple [p103, p104, p105]
    let p107 = Types.Var s3 p106
    let p108 = Types.Var s2 p107
    let p109 = Types.Var s1 p108

    -- ################################# NEGATIVE CASES #################################
    --
    -- TestCase 1
    -- Identifier binding fails; Variable out of scope
    let n1 = Types.Var s1 Types.Skip
    let n2 = Types.BindIdent s2 s1
    let n3 = Types.Multiple [n1, n2]
    let n4 = Types.Var s2 n3

    -- TestCase 2
    -- Unification fails (due to unequal values)
    let n5 = Types.BindValue s1 $ Types.Expr $ Types.Lit 5
    let n6 = Types.BindValue s2 $ Types.Expr $ Types.Lit 7
    let n7 = Types.BindIdent s1 s2
    let n8 = Types.Multiple [n5, n6, n7]
    let n9 = Types.Var s1 n8
    let n10 = Types.Var s2 n9

    -- TestCase 3
    -- Binding two records fails (different arity)
    let n11 = Types.BindValue s1 $ Types.Record 12 $ Map.fromList [(1, s2), (2, s3)]
    let n12 = Types.BindValue s4 $ Types.Record 12 $ Map.fromList [(1, s5)]
    let n13 = Types.BindIdent s1 s4
    let n14 = Types.Multiple [n11, n12, n13]
    let n15 = Types.Var s1 n14
    let n16 = Types.Var s2 n15
    let n17 = Types.Var s3 n16
    let n18 = Types.Var s4 n17
    let n19 = Types.Var s5 n18

    -- TestCase 4
    -- Conditional expression is not a literal
    let n20 = Types.BindValue s1 $ Types.Record 12 $ Map.fromList [(10, s2), (12, s3)]
    let n21 = Types.BindValue s4 $ Types.Expr $ Types.Lit 1
    let n22 = Types.BindValue s4 $ Types.Expr $ Types.Lit 2
    let n23 = Types.Conditional s1 n21 n22
    let n24 = Types.Multiple [n20, n23]
    let n25 = Types.Var s1 n24
    let n26 = Types.Var s2 n25
    let n27 = Types.Var s3 n26
    let n28 = Types.Var s4 n27

    -- TestCase 5
    -- Pattern in a case statement is not record
    let n29 = Types.BindValue s1 $ Types.Expr $ Types.Lit 100
    let n30 = Types.BindValue s2 $ Types.Expr $ Types.Lit 10
    let n31 = Types.BindValue s2 $ Types.Expr $ Types.Lit 1000
    let n32 = Types.Match s1 (Types.Expr $ Types.Lit 10) n30 n31
    let n33 = Types.Multiple [n29, n32]
    let n34 = Types.Var s2 n33
    let n35 = Types.Var s1 n34

    -- TestCase 6
    -- Invalid Procedure Call (Type is not a closure)
    let n36 = Types.BindValue s1 $ Types.Expr $ Types.Lit 10
    let n37 = Types.Apply s1 [s2]
    let n38 = Types.Multiple [n36, n37]
    let n39 = Types.Var s2 n38
    let n40 = Types.Var s1 n39

    -- TestCase 7
    -- Invalid Procedure Call (Arguments arity mistmatch)
    let n41 = Types.BindIdent "y" "x"
    let n42 = Types.BindValue s1 $ Types.Proc ["x", "y"] n41
    let n43 = Types.Apply s1 [s2]
    let n44 = Types.Multiple [n42, n43]
    let n45 = Types.Var s2 n44
    let n46 = Types.Var s1 n45

    -- TestCase 8
    -- Evaluating Expression Fails (one operand is not bound to a value)
    let n47 = Types.BindValue s1 $ Types.Expr $ Types.Lit 2
    let n48 = Types.BindValue s3 $ Types.Expr $ Types.Exp Types.Sub (Types.Variable s1) (Types.Variable s2)
    let n49 = Types.Multiple [n47, n48]
    let n50 = Types.Var s3 n49
    let n51 = Types.Var s2 n50
    let n52 = Types.Var s1 n51

    putStrLn "\n\n"

    -- ###################################################################
    --                          RUNNING TESTCASES
    -- ###################################################################
    --
    -- ################################# POSITIVE CASES #################################

    putStrLn "###################################################################"
    putStrLn "                  RUNNING POSITIVE TESTCASES"
    putStrLn "###################################################################"
    
    putStrLn "\n\n"
    let (x, y) = Ex.executeProgram p5
    print p5
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (x, y) = Ex.executeProgram p8
    print p8
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (x, y) = Ex.executeProgram p13
    print p13
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (x, y) = Ex.executeProgram p19
    print p19
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (x, y) = Ex.executeProgram p25
    print p25
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (x, y) = Ex.executeProgram p29
    print p29
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (x, y) = Ex.executeProgram p31
    print p31
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (x, y) = Ex.executeProgram p36
    print p36
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (x, y) = Ex.executeProgram p45
    print p45
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (x, y) = Ex.executeProgram p52
    print p52
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (x, y) = Ex.executeProgram p59
    print p59
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------"

    putStrLn "\n\n"
    let (x, y) = Ex.executeProgram p68
    print p68
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (x, y) = Ex.executeProgram p77
    print p77
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (x, y) = Ex.executeProgram p85
    print p85
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (x, y) = Ex.executeProgram p97
    print p97
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------"
    
    putStrLn "\n\n"
    let (x, y) = Ex.executeProgram p102
    print p102
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------"

    putStrLn "\n\n"
    let (x, y) = Ex.executeProgram p109
    print p109
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------"
    putStrLn "\n\n"

    -- ################################# NEGATIVE CASES #################################

    putStrLn "###################################################################"
    putStrLn "                    RUNNING NEGATIVE TESTCASES"
    putStrLn "###################################################################"
    
    -- Failure Case
    putStrLn "\n\n"
    let (x, y) = Ex.executeProgram n4
    print n4
    print y
    Control.Exception.catch (print x) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    putStrLn "-----------------------PASSED---------------------------" 

    -- Failure Case
    putStrLn "\n\n"
    let (x, y) = Ex.executeProgram n10
    print n10
    print y
    Control.Exception.catch (print x) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    putStrLn "-----------------------PASSED---------------------------" 

    -- Failure Case
    putStrLn "\n\n"
    let (x, y) = Ex.executeProgram n19
    print n19
    print y
    Control.Exception.catch (print x) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    putStrLn "-----------------------PASSED---------------------------" 

    -- Failure case
    putStrLn "\n\n"
    let (x, y) = Ex.executeProgram n28
    print n28
    Control.Exception.catch (print y) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print x) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    putStrLn "-----------------------PASSED---------------------------"

    -- Failure case
    putStrLn "\n\n"
    let (x, y) = Ex.executeProgram n35
    print n35
    Control.Exception.catch (print y) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print x) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    putStrLn "-----------------------PASSED---------------------------"

    -- Failure case
    putStrLn "\n\n"
    let (x, y) = Ex.executeProgram n40
    print n40
    Control.Exception.catch (print y) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print x) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    putStrLn "-----------------------PASSED---------------------------"

    -- Failure case
    putStrLn "\n\n"
    let (x, y) = Ex.executeProgram n46
    print n46
    Control.Exception.catch (print y) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print x) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    putStrLn "-----------------------PASSED---------------------------"

    -- Failure case
    putStrLn "\n\n"
    let (x, y) = Ex.executeProgram n52
    print n52
    print y
    Control.Exception.catch (print x) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    putStrLn "-----------------------PASSED---------------------------"
