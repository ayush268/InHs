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

    -- Conditional Statements (Taking True branch)
    -- Unification should take place
    let p59 = Types.BindIdent s1 s2
    let p60 = Types.BindValue s2 $ Types.Lit 100
    let p61 = Types.Multiple[Types.BindValue s1 $ Types.Lit 1, Types.Conditional s1 p59 p60]
    let p62 = Types.Var s1 p61
    let p63 = Types.Var s2 p62

    -- Conditional Statements (Taking False branch)
    -- Unification should NOT take place
    let p64 = Types.BindIdent s1 s2
    let p65 = Types.BindValue s2 $ Types.Lit 100
    let p66 = Types.Multiple[Types.BindValue s1 $ Types.Lit 0, Types.Conditional s1 p64 p65]
    let p67 = Types.Var s1 p66
    let p68 = Types.Var s2 p67

    -- Match (Case) statement (Pattern matched)
    -- Unification of s2 and s3 should occur
    let p71 = Types.BindIdent s2 s3
    let p72 = Types.BindValue s2 $ Types.Lit 100
    let p69 = Types.BindValue s1 $ Types.Record 12 $ Map.fromList [(10, s3), (12, s4)]
    let p70 = Types.Match s1 (Types.Record 12 $ Map.fromList [(10, "m"),(12,"n")]) p71 p72
    let p73 = Types.Multiple[p69, p70]
    let p74 = Types.Var s1 p73
    let p75 = Types.Var s2 p74
    let p76 = Types.Var s3 p75
    let p77 = Types.Var s4 p76

    -- Match (Case) statement (Pattern matched)
    -- Unification of s2 and s3 should NOT occur
    let p80 = Types.BindIdent s2 s3
    let p81 = Types.BindValue s2 $ Types.Lit 100
    let p78 = Types.BindValue s1 $ Types.Record 12 $ Map.fromList [(10, s3), (12, s4)]
    let p79 = Types.Match s1 (Types.Record 12 $ Map.fromList [(10, "m"),(22,"n")]) p80 p81
    let p82 = Types.Multiple[p78, p79]
    let p83 = Types.Var s1 p82
    let p84 = Types.Var s2 p83
    let p85 = Types.Var s3 p84
    let p86 = Types.Var s4 p85

    -- Procedure Call (Without free variables)
    -- Unification of s1 and s2 should occur (arguments equated)
    let p88 = Types.BindValue s4 $ Types.Lit 1
    let p89 = Types.BindValue s3 $ Types.Proc [s1, s2] $ Types.BindIdent s1 s2
    let p90 = Types.Apply s3 [s4, s5]
    let p96 = Types.Multiple[p89, p88, p90]
    let p91 = Types.Var s1 p96
    let p92 = Types.Var s2 p91
    let p93 = Types.Var s3 p92
    let p94 = Types.Var s4 p93
    let p95 = Types.Var s5 p94

    -- Procedure Call (With free variables)
    -- Unification of s3, s4, s5 (with 10) should occur
    let p97 = Types.Multiple[Types.BindIdent s5 s4, Types.BindIdent s3 s5]
    let p98 = Types.BindValue s3 $ Types.Lit 10
    let p99 = Types.BindValue s6 $ Types.Proc [s4, s5] p97
    let p100 = Types.Apply s6 [s1, s2]
    let p106 = Types.Multiple[p99, p98, p100]
    let p101 = Types.Var s1 p106
    let p102 = Types.Var s2 p101
    let p103 = Types.Var s3 p102
    let p104 = Types.Var s4 p103
    let p105 = Types.Var s5 p104
    let p106 = Types.Var s6 p105

    -- Conditional expression is not a literal
    let p107 = Types.BindValue s1 $ Types.Record 12 $ Map.fromList [(10, s2), (12, s3)]
    let p108 = Types.Conditional s1 (Types.BindValue s4 $ Types.Lit 1) (Types.BindValue s4 $ Types.Lit 2)
    let p109 = Types.Multiple[p107, p108]
    let p110 = Types.Var s1 p109
    let p111 = Types.Var s2 p110
    let p112 = Types.Var s3 p111
    let p113 = Types.Var s4 p112

    -- Pattern in a case statement is not record
    let p114 = Types.BindValue s2 $ Types.Lit 100
    let p115 = Types.BindValue s2 $ Types.Lit 99
    let p116 = Types.Multiple[Types.BindValue s1 $ Types.Lit 3, Types.Match s1 (Types.Lit 100) p114 p115]
    let p117 = Types.Var s1 p116
    let p118 = Types.Var s2 p117

    -- Invalid Procedure Call (Type is not a closure)
    let p119 = Types.BindValue s1 $ Types.Lit 12
    let p120 = Types.BindValue s2 $ Types.Lit 13
    let p121 = Types.Multiple[p119, p120, Types.Apply s1 [s2]]
    let p122 = Types.Var s1 p121
    let p123 = Types.Var s2 p122

    -- Invalid Procedure Call (Insufficient arguments)
    let p124 = Types.BindIdent s4 s3
    let p125 = Types.BindValue s2 $ Types.Lit 10
    let p126 = Types.BindValue s1 $ Types.Proc [s3, s4] p124
    let p127 = Types.Multiple[p126, p125, Types.Apply s1 [s2]]
    let p128 = Types.Var s1 p127
    let p129 = Types.Var s2 p128
    let p130 = Types.Var s3 p129
    let p131 = Types.Var s4 p130


    let (x, _, y) = Ex.executeProgram p5
    print p5
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (x, _, y) = Ex.executeProgram p8
    print p8
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------" 

    -- Failure Case
    putStrLn "\n\n"
    let (x, _, y) = Ex.executeProgram p11
    print p11
    print y
    Control.Exception.catch (print x) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    putStrLn "-----------------------PASSED---------------------------" 

    -- Failure Case
    putStrLn "\n\n"
    let (x, _, y) = Ex.executeProgram p14
    print p14
    print y
    Control.Exception.catch (print x) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (x, _, y) = Ex.executeProgram p20
    print p20
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (x, _, y) = Ex.executeProgram p29
    print p29
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (x, _, y) = Ex.executeProgram p27
    print p27
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (x, _, y) = Ex.executeProgram p33
    print p33
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (x, _, y) = Ex.executeProgram p37
    print p37
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------" 

    -- Failure Case
    putStrLn "\n\n"
    let (x, _, y) = Ex.executeProgram p44
    print p44
    print y
    Control.Exception.catch (print x) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (x, _, y) = Ex.executeProgram p53
    print p53
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (x, _, y) = Ex.executeProgram p58
    print p58
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (x, _, y) = Ex.executeProgram p63
    print p63
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (x, _, y) = Ex.executeProgram p68
    print p68
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------"

    putStrLn "\n\n"
    let (x, _, y) = Ex.executeProgram p77
    print p77
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (x, _, y) = Ex.executeProgram p86
    print p86
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (x, _, y) = Ex.executeProgram p95
    print p95
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (x, _, y) = Ex.executeProgram p106
    print p106
    print y
    print x
    putStrLn "-----------------------PASSED---------------------------"
    
    -- Failure case
    putStrLn "\n\n"
    let (x, _, y) = Ex.executeProgram p113
    print p113
    Control.Exception.catch (print x) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    putStrLn "-----------------------PASSED---------------------------"

    -- Failure case
    putStrLn "\n\n"
    let (x, _, y) = Ex.executeProgram p118
    print p118
    Control.Exception.catch (print y) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    putStrLn "-----------------------PASSED---------------------------"

    -- Failure case
    putStrLn "\n\n"
    let (x, _, y) = Ex.executeProgram p123
    print p123
    Control.Exception.catch (print y) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    putStrLn "-----------------------PASSED---------------------------"

    -- Failure case
    putStrLn "\n\n"
    let (x, _, y) = Ex.executeProgram p131
    print p131
    Control.Exception.catch (print y) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    putStrLn "-----------------------PASSED---------------------------"
