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

    -- TestCase 18
    -- Thread Statements (without Suspend Cases)
    let p110 = Types.Thread $ Types.BindValue s1 $ Types.Expr $ Types.Lit 2
    let p111 = Types.Thread $ Types.BindIdent s2 s1
    let p112 = Types.Multiple [p110, p111]
    let p113 = Types.Var s2 p112
    let p114 = Types.Var s1 p113

    -- TestCase 19
    -- Thread Statement (with Suspend case of Conditional)
    let p115 = Types.Thread $ Types.BindIdent s3 s1
    let p116 = Types.BindValue s3 $ Types.Expr $ Types.Lit 0
    let p117 = Types.BindIdent s2 s1
    let p118 = Types.BindValue s2 $ Types.Expr $ Types.Lit 100
    let p119 = Types.Conditional s1 p117 p118
    let p120 = Types.Var s3 $ Types.Multiple [p115, p116, p119]
    let p121 = Types.Var s2 p120
    let p122 = Types.Var s1 p121

    -- TestCase 20
    -- Thread Statement (with Suspend Back and Forth)
    -- Original Thread - suspends,
    -- Second Thread - executes, binds s1 and suspends
    -- Original Thread - continues, binds F and completes
    -- Second Thread - continues, calls F and completes
    let p123 = Types.BindValue s4 $ Types.Expr $ Types.Lit 0
    let p124 = Types.BindIdent s1 s4
    let p125 = Types.Apply "F" [s1, s2]
    let p126 = Types.Thread $ Types.Multiple [p124, p125]
    let p127 = Types.BindValue s5 $ Types.Expr $ Types.Lit 10
    let p128 = Types.BindValue s5 $ Types.Expr $ Types.Lit 25
    let p129 = Types.Conditional s1 p127 p128
    let p130 = Types.BindIdent "Y" "X"
    let p131 = Types.BindIdent s3 "Y"
    let p132 = Types.BindValue "F" $ Types.Proc ["X", "Y"] $ Types.Multiple [p130, p131]
    let p133 = Types.Multiple [p123, p126, p129, p132]
    let p134 = Types.Var s5 p133
    let p135 = Types.Var s4 p134
    let p136 = Types.Var s3 p135
    let p137 = Types.Var s2 p136
    let p138 = Types.Var s1 p137
    let p139 = Types.Var "F" p138

    -- TestCase 21
    -- ByNeed Statement (trigger activated by a variable being bound)
    let p140 = Types.BindValue "m" $ Types.Expr $ Types.Lit 3
    let p141 = Types.BindValue "n" $ Types.Expr $ Types.Lit 2
    let p142 = Types.ByNeed "x" $ Types.Proc ["a"] (Types.BindValue "a" $ Types.Record 12 $ Map.fromList [(1,"y"), (2,"m")])
    let p143 = Types.ByNeed "x" $ Types.Proc ["b"] (Types.BindValue "b" $ Types.Record 12 $ Map.fromList [(1,"n"), (2,"z")])
    let p144 = Types.BindValue "x" $ Types.Record 12 $ Map.fromList [(1,"y"), (2,"z")]
    let p145 = Types.Var "x" $ Types.Var "y" $ Types.Var "z" $ Types.Var "m" $ Types.Var "n" $ Types.Multiple [p140, p141, p142, p143, p144]

    -- TestCase 22
    -- ByNeed Statement (trigger activated by a suspendable statement)
    let p146 = Types.ByNeed "x" $ Types.Proc ["a"] (Types.BindValue "a" $ Types.Expr $ Types.Lit 0)
    let p147 = Types.Conditional "x" (Types.BindValue "y" $ Types.Expr $ Types.Lit 1) (Types.BindValue "y" $ Types.Expr $ Types.Lit 2)
    let p148 = Types.Var "x" $ Types.Var "y" $ Types.Multiple [p146, p147]

    -- TestCase 23
    -- ByNeed Statement (trigger NOT activated)
    let p149 = Types.ByNeed "x" $ Types.Proc ["a"] (Types.BindValue "a" $ Types.Expr $ Types.Lit 2)
    let p150 = Types.BindIdent "x" "y"
    let p151 = Types.Var "x" $ Types.Var "y" $ Types.Multiple [p149, p150]

    -- TestCase 24
    -- Normal (Eager) Factorial Function
    let p152 = Types.Multiple [(Types.BindValue "s" $ Types.Expr $ Types.Exp Types.Sub (Types.Variable "n") (Types.Lit 1)), (Types.Apply "fact" ["s","t"]), (Types.BindValue "a" $ Types.Expr $ Types.Exp Types.Mult (Types.Variable "n") (Types.Variable "t"))]
    let p153 = Types.BindValue "a" $ Types.Expr $ Types.Lit 1
    let p154 = Types.Conditional "n" p152 p153
    let p155 = Types.Var "s" $ Types.Var "t" p154
    let p156 = Types.BindValue "fact" $ Types.Proc ["n", "a"] p155
    let p157 = Types.Multiple [p156, (Types.BindValue "input" $ Types.Expr $ Types.Lit 10), (Types.Apply "fact" ["input", "x"]), (Types.BindValue "y" $ Types.Expr $ Types.Exp Types.Add (Types.Variable "x") (Types.Lit 0))]
    let p158 = Types.Var "y" $ Types.Var "x" $ Types.Var "fact" $ Types.Var "input" p157


    -- TestCase 25
    -- Lazy Factorial Function
    let p159 = Types.Multiple [(Types.BindValue "s" $ Types.Expr $ Types.Exp Types.Sub (Types.Variable "n") (Types.Lit 1)), (Types.Apply "fact" ["s","t"]), (Types.BindValue "a" $ Types.Expr $ Types.Exp Types.Mult (Types.Variable "n") (Types.Variable "t"))]
    let p160 = Types.BindValue "a" $ Types.Expr $ Types.Lit 1
    let p161 = Types.Conditional "n" p159 p160
    let p162 = Types.ByNeed "a" $ Types.Proc ["b"] p161
    let p163 = Types.Var "s" $ Types.Var "t" p162
    let p164 = Types.BindValue "fact" $ Types.Proc ["n", "a"] p163
    let p165 = Types.Multiple [p164, (Types.BindValue "input" $ Types.Expr $ Types.Lit 10), (Types.Apply "fact" ["input", "x"]), (Types.BindValue "y" $ Types.Expr $ Types.Exp Types.Add (Types.Variable "x") (Types.Lit 0))]
    let p166 = Types.Var "y" $ Types.Var "x" $ Types.Var "fact" $ Types.Var "input" p165

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

    -- TestCase 9
    -- Single Suspended Statement
    -- (Should Fail since cannot be bound)
    let n53 = Types.BindIdent s2 s1
    let n54 = Types.BindValue s2 $ Types.Expr $ Types.Lit 100
    let n55 = Types.Conditional s1 n53 n54
    let n56 = Types.Var s1 n55
    let n57 = Types.Var s2 n56

    -- TestCase 10
    -- Multiple Statements suspended on each other
    -- 3 different Thread (each will end up suspended, thus not halt)
    let n58 = Types.Thread $ Types.Apply s3 []
    let n59 = Types.BindValue s1 $ Types.Expr $ Types.Lit 1
    let n60 = Types.BindValue s1 $ Types.Expr $ Types.Lit 0
    let n61 = Types.Match s2 (Types.Record 10 $ Map.fromList [(1, s3)]) n59 n60
    let n62 = Types.Thread n61
    let n63 = Types.BindValue s2 $ Types.Record 10 $ Map.fromList [(1, s1)]
    let n64 = Types.BindValue s2 $ Types.Record 10 $ Map.fromList [(1, s3)]
    let n65 = Types.Conditional s1 n63 n64
    let n66 = Types.Multiple [n58, n62, n65]
    let n67 = Types.Var s3 n66
    let n68 = Types.Var s2 n67
    let n69 = Types.Var s1 n68

    -- TestCase 11
    -- ByNeed Statement trigger activated and Unification error
    let n70 = Types.ByNeed "x" $ Types.Proc ["a"] (Types.BindValue "a" $ Types.Expr $ Types.Lit 2)
    let n71 = Types.BindIdent "x" "y"
    let n72 = Types.BindValue "y" $ Types.Expr $ Types.Lit 4
    let n73 = Types.Multiple [n70, n71, n72]
    let n74 = Types.Var "y" n73
    let n75 = Types.Var "x" n74

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
    let (a, b, c, d, e) = Ex.executeProgram p5
    print p5
    print a
    print b
    print c
    print d
    print e
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (a, b, c, d, e) = Ex.executeProgram p8
    print p8
    print a
    print b
    print c
    print d
    print e
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (a, b, c, d, e) = Ex.executeProgram p13
    print p13
    print a
    print b
    print c
    print d
    print e
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (a, b, c, d, e) = Ex.executeProgram p19
    print p19
    print a
    print b
    print c
    print d
    print e
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (a, b, c, d, e) = Ex.executeProgram p25
    print p25
    print a
    print b
    print c
    print d
    print e
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (a, b, c, d, e) = Ex.executeProgram p29
    print p29
    print a
    print b
    print c
    print d
    print e
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (a, b, c, d, e) = Ex.executeProgram p31
    print p31
    print a
    print b
    print c
    print d
    print e
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (a, b, c, d, e) = Ex.executeProgram p36
    print p36
    print a
    print b
    print c
    print d
    print e
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (a, b, c, d, e) = Ex.executeProgram p45
    print p45
    print a
    print b
    print c
    print d
    print e
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (a, b, c, d, e) = Ex.executeProgram p52
    print p52
    print a
    print b
    print c
    print d
    print e
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (a, b, c, d, e) = Ex.executeProgram p59
    print p59
    print a
    print b
    print c
    print d
    print e
    putStrLn "-----------------------PASSED---------------------------"

    putStrLn "\n\n"
    let (a, b, c, d, e) = Ex.executeProgram p68
    print p68
    print a
    print b
    print c
    print d
    print e
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (a, b, c, d, e) = Ex.executeProgram p77
    print p77
    print a
    print b
    print c
    print d
    print e
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (a, b, c, d, e) = Ex.executeProgram p85
    print p85
    print a
    print b
    print c
    print d
    print e
    putStrLn "-----------------------PASSED---------------------------" 

    putStrLn "\n\n"
    let (a, b, c, d, e) = Ex.executeProgram p97
    print p97
    print a
    print b
    print c
    print d
    print e
    putStrLn "-----------------------PASSED---------------------------"
    
    putStrLn "\n\n"
    let (a, b, c, d, e) = Ex.executeProgram p102
    print p102
    print a
    print b
    print c
    print d
    print e
    putStrLn "-----------------------PASSED---------------------------"

    putStrLn "\n\n"
    let (a, b, c, d, e) = Ex.executeProgram p109
    print p109
    print a
    print b
    print c
    print d
    print e
    putStrLn "-----------------------PASSED---------------------------"

    putStrLn "\n\n"
    let (a, b, c, d, e) = Ex.executeProgram p114
    print p114
    print a
    print b
    print c
    print d
    print e
    putStrLn "-----------------------PASSED---------------------------"

    putStrLn "\n\n"
    let (a, b, c, d, e) = Ex.executeProgram p122
    print p122
    print a
    print b
    print c
    print d
    print e
    putStrLn "-----------------------PASSED---------------------------"

    putStrLn "\n\n"
    let (a, b, c, d, e) = Ex.executeProgram p139
    print p139
    print a
    print b
    print c
    print d
    print e
    putStrLn "-----------------------PASSED---------------------------"

    putStrLn "\n\n"
    let (a, b, c, d, e) = Ex.executeProgram p145
    print p145
    print a
    print b
    print c
    print d
    print e
    putStrLn "-----------------------PASSED---------------------------"

    putStrLn "\n\n"
    let (a, b, c, d, e) = Ex.executeProgram p148
    print p148
    print a
    print b
    print c
    print d
    print e
    putStrLn "-----------------------PASSED---------------------------"

    putStrLn "\n\n"
    let (a, b, c, d, e) = Ex.executeProgram p151
    print p151
    print a
    print b
    print c
    print d
    print e
    putStrLn "-----------------------PASSED---------------------------"

    putStrLn "\n\n"
    let (a, b, c, d, e) = Ex.executeProgram p158
    print p158
    print a
    print b
    print c
    print d
    print e
    putStrLn "-----------------------PASSED---------------------------"

    putStrLn "\n\n"
    let (a, b, c, d, e) = Ex.executeProgram p166
    print p166
    print a
    print b
    print c
    print d
    print e
    putStrLn "-----------------------PASSED---------------------------"

    putStrLn "\n\n"

    -- ################################# NEGATIVE CASES #################################

    putStrLn "###################################################################"
    putStrLn "                    RUNNING NEGATIVE TESTCASES"
    putStrLn "###################################################################"
    
    -- Failure Case
    putStrLn "\n\n"
    let (a, b, c, d, e) = Ex.executeProgram n4
    print n4
    Control.Exception.catch (print a) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print b) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print c) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print d) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print e) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    putStrLn "-----------------------PASSED---------------------------" 

    -- Failure Case
    putStrLn "\n\n"
    let (a, b, c, d, e) = Ex.executeProgram n10
    print n10
    Control.Exception.catch (print a) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print b) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print c) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print d) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print e) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    putStrLn "-----------------------PASSED---------------------------" 

    -- Failure Case
    putStrLn "\n\n"
    let (a, b, c, d, e) = Ex.executeProgram n19
    print n19
    Control.Exception.catch (print a) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print b) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print c) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print d) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print e) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    putStrLn "-----------------------PASSED---------------------------" 

    -- Failure case
    putStrLn "\n\n"
    let (a, b, c, d, e) = Ex.executeProgram n28
    print n28
    Control.Exception.catch (print a) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print b) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print c) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print d) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print e) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    putStrLn "-----------------------PASSED---------------------------"

    -- Failure case
    putStrLn "\n\n"
    let (a, b, c, d, e) = Ex.executeProgram n35
    print n35
    Control.Exception.catch (print a) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print b) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print c) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print d) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print e) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    putStrLn "-----------------------PASSED---------------------------"

    -- Failure case
    putStrLn "\n\n"
    let (a, b, c, d, e) = Ex.executeProgram n40
    print n40
    Control.Exception.catch (print a) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print b) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print c) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print d) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print e) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    putStrLn "-----------------------PASSED---------------------------"

    -- Failure case
    putStrLn "\n\n"
    let (a, b, c, d, e) = Ex.executeProgram n46
    print n46
    Control.Exception.catch (print a) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print b) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print c) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print d) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print e) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    putStrLn "-----------------------PASSED---------------------------"

    -- Failure case
    putStrLn "\n\n"
    let (a, b, c, d, e) = Ex.executeProgram n52
    print n52
    Control.Exception.catch (print a) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print b) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print c) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print d) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print e) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    putStrLn "-----------------------PASSED---------------------------"

    -- Failure case
    putStrLn "\n\n"
    let (a, b, c, d, e) = Ex.executeProgram n57
    print n57
    Control.Exception.catch (print a) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print b) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print c) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print d) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print e) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    putStrLn "-----------------------PASSED---------------------------"

    -- Failure case
    putStrLn "\n\n"
    let (a, b, c, d, e) = Ex.executeProgram n69
    print n69
    Control.Exception.catch (print a) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print b) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print c) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print d) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print e) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    putStrLn "-----------------------PASSED---------------------------"

    -- Failure case
    putStrLn "\n\n"
    let (a, b, c, d, e) = Ex.executeProgram n75
    print n75
    Control.Exception.catch (print a) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print b) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print c) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print d) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    Control.Exception.catch (print e) (\msg -> putStrLn $ "Caught " ++ show (msg::Control.Exception.SomeException))
    putStrLn "-----------------------PASSED---------------------------"
