module OpsTests

open Expecto
open System
open FSharp.Stats
open TestExtensions

[<Tests>]
let constantTests =
    testList "Ops.constants" [
        testCase "pi constant" <| fun () ->
            Expect.floatClose Accuracy.medium Ops.pi Math.PI "Ops.pi should equal Math.PI"
        
        testCase "inf constant" <| fun () ->
            Expect.equal Ops.inf Double.PositiveInfinity "Ops.inf should equal positive infinity"
        
        testCase "infNeg constant" <| fun () ->
            Expect.equal Ops.infNeg Double.NegativeInfinity "Ops.infNeg should equal negative infinity"
        
        testCase "epsilon constant" <| fun () ->
            Expect.equal Ops.epsilon 1.11022302462515654042e-16 "Ops.epsilon should have correct value"
        
        testCase "logMax constant" <| fun () ->
            Expect.equal Ops.logMax 7.09782712893383996732E2 "Ops.logMax should have correct value"
        
        testCase "logMin constant" <| fun () ->
            Expect.equal Ops.logMin -7.451332191019412076235E2 "Ops.logMin should have correct value"
    ]

[<Tests>]
let log2Tests =
    testList "Ops.log2" [
        testCase "log2 of 1" <| fun () ->
            let result = Ops.log2 1.0
            Expect.floatClose Accuracy.high result 0.0 "log2(1) should be 0"
        
        testCase "log2 of 2" <| fun () ->
            let result = Ops.log2 2.0
            Expect.floatClose Accuracy.high result 1.0 "log2(2) should be 1"
        
        testCase "log2 of 4" <| fun () ->
            let result = Ops.log2 4.0
            Expect.floatClose Accuracy.high result 2.0 "log2(4) should be 2"
        
        testCase "log2 of 8" <| fun () ->
            let result = Ops.log2 8.0
            Expect.floatClose Accuracy.high result 3.0 "log2(8) should be 3"
        
        testCase "log2 of 0.5" <| fun () ->
            let result = Ops.log2 0.5
            Expect.floatClose Accuracy.high result -1.0 "log2(0.5) should be -1"
        
        testCase "log2 of 1024" <| fun () ->
            let result = Ops.log2 1024.0
            Expect.floatClose Accuracy.high result 10.0 "log2(1024) should be 10"
    ]

[<Tests>]
let revLog2Tests =
    testList "Ops.revLog2" [
        testCase "revLog2 of 0" <| fun () ->
            let result = Ops.revLog2 0.0
            Expect.floatClose Accuracy.high result 1.0 "2^0 should be 1"
        
        testCase "revLog2 of 1" <| fun () ->
            let result = Ops.revLog2 1.0
            Expect.floatClose Accuracy.high result 2.0 "2^1 should be 2"
        
        testCase "revLog2 of 2" <| fun () ->
            let result = Ops.revLog2 2.0
            Expect.floatClose Accuracy.high result 4.0 "2^2 should be 4"
        
        testCase "revLog2 of 3" <| fun () ->
            let result = Ops.revLog2 3.0
            Expect.floatClose Accuracy.high result 8.0 "2^3 should be 8"
        
        testCase "revLog2 of 10" <| fun () ->
            let result = Ops.revLog2 10.0
            Expect.floatClose Accuracy.high result 1024.0 "2^10 should be 1024"
        
        testCase "revLog2 of negative value" <| fun () ->
            let result = Ops.revLog2 -1.0
            Expect.floatClose Accuracy.high result 0.5 "2^-1 should be 0.5"
        
        testCase "revLog2 roundtrip" <| fun () ->
            let original = 16.0
            let result = Ops.revLog2 (Ops.log2 original)
            Expect.floatClose Accuracy.high result original "log2 and revLog2 should be inverses"
    ]

[<Tests>]
let isNanTests =
    testList "Ops.isNan" [
        testCase "isNan for NaN" <| fun () ->
            let result = Ops.isNan Double.NaN
            Expect.isTrue result "NaN should be detected as NaN"
        
        testCase "isNan for regular number" <| fun () ->
            let result = Ops.isNan 42.0
            Expect.isFalse result "Regular number should not be NaN"
        
        testCase "isNan for zero" <| fun () ->
            let result = Ops.isNan 0.0
            Expect.isFalse result "Zero should not be NaN"
        
        testCase "isNan for infinity" <| fun () ->
            let result = Ops.isNan Double.PositiveInfinity
            Expect.isFalse result "Infinity should not be NaN"
        
        testCase "isNan for negative infinity" <| fun () ->
            let result = Ops.isNan Double.NegativeInfinity
            Expect.isFalse result "Negative infinity should not be NaN"
    ]

[<Tests>]
let isInfTests =
    testList "Ops.isInf" [
        testCase "isInf for positive infinity" <| fun () ->
            let result = Ops.isInf Double.PositiveInfinity
            Expect.isTrue result "Positive infinity should be detected as infinity"
        
        testCase "isInf for negative infinity" <| fun () ->
            let result = Ops.isInf Double.NegativeInfinity
            Expect.isTrue result "Negative infinity should be detected as infinity"
        
        testCase "isInf for regular number" <| fun () ->
            let result = Ops.isInf 42.0
            Expect.isFalse result "Regular number should not be infinity"
        
        testCase "isInf for NaN" <| fun () ->
            let result = Ops.isInf Double.NaN
            Expect.isFalse result "NaN should not be infinity"
        
        testCase "isInf for zero" <| fun () ->
            let result = Ops.isInf 0.0
            Expect.isFalse result "Zero should not be infinity"
    ]

[<Tests>]
let isPosInfTests =
    testList "Ops.isPosInf" [
        testCase "isPosInf for positive infinity" <| fun () ->
            let result = Ops.isPosInf Double.PositiveInfinity
            Expect.isTrue result "Positive infinity should be detected"
        
        testCase "isPosInf for negative infinity" <| fun () ->
            let result = Ops.isPosInf Double.NegativeInfinity
            Expect.isFalse result "Negative infinity should not be positive infinity"
        
        testCase "isPosInf for regular number" <| fun () ->
            let result = Ops.isPosInf 42.0
            Expect.isFalse result "Regular number should not be positive infinity"
        
        testCase "isPosInf for large positive number" <| fun () ->
            let result = Ops.isPosInf Double.MaxValue
            Expect.isFalse result "MaxValue should not be positive infinity"
    ]

[<Tests>]
let isNegInfTests =
    testList "Ops.isNegInf" [
        testCase "isNegInf for negative infinity" <| fun () ->
            let result = Ops.isNegInf Double.NegativeInfinity
            Expect.isTrue result "Negative infinity should be detected"
        
        testCase "isNegInf for positive infinity" <| fun () ->
            let result = Ops.isNegInf Double.PositiveInfinity
            Expect.isFalse result "Positive infinity should not be negative infinity"
        
        testCase "isNegInf for regular number" <| fun () ->
            let result = Ops.isNegInf -42.0
            Expect.isFalse result "Regular negative number should not be negative infinity"
        
        testCase "isNegInf for large negative number" <| fun () ->
            let result = Ops.isNegInf Double.MinValue
            Expect.isFalse result "MinValue should not be negative infinity"
    ]

[<Tests>]
let squareTests =
    testList "Ops.square" [
        testCase "square of positive number" <| fun () ->
            let result = Ops.square 5.0
            Expect.floatClose Accuracy.high result 25.0 "5^2 should be 25"
        
        testCase "square of negative number" <| fun () ->
            let result = Ops.square -3.0
            Expect.floatClose Accuracy.high result 9.0 "(-3)^2 should be 9"
        
        testCase "square of zero" <| fun () ->
            let result = Ops.square 0.0
            Expect.floatClose Accuracy.high result 0.0 "0^2 should be 0"
        
        testCase "square of one" <| fun () ->
            let result = Ops.square 1.0
            Expect.floatClose Accuracy.high result 1.0 "1^2 should be 1"
        
        testCase "square of decimal" <| fun () ->
            let result = Ops.square 0.5
            Expect.floatClose Accuracy.high result 0.25 "0.5^2 should be 0.25"
        
        testCase "square of integer type" <| fun () ->
            let result = Ops.square 7
            Expect.equal result 49 "7^2 should be 49"
    ]

[<Tests>]
let arsinhTests =
    testList "Ops.arsinh" [
        testCase "arsinh of 0" <| fun () ->
            let result = Ops.arsinh 0.0
            Expect.floatClose Accuracy.high result 0.0 "arsinh(0) should be 0"
        
        testCase "arsinh of 1" <| fun () ->
            let result = Ops.arsinh 1.0
            let expected = Math.Log(1.0 + sqrt(2.0))
            Expect.floatClose Accuracy.medium result expected "arsinh(1) should match formula"
        
        testCase "arsinh of positive number" <| fun () ->
            let result = Ops.arsinh 5.0
            let expected = Math.Log(5.0 + sqrt(26.0))
            Expect.floatClose Accuracy.medium result expected "arsinh(5) should match formula"
        
        testCase "arsinh of negative number" <| fun () ->
            let result = Ops.arsinh -2.0
            let expected = Math.Log(-2.0 + sqrt(5.0))
            Expect.floatClose Accuracy.medium result expected "arsinh(-2) should match formula"
        
        testCase "arsinh is odd function" <| fun () ->
            let x = 3.5
            let positiveResult = Ops.arsinh x
            let negativeResult = Ops.arsinh -x
            Expect.floatClose Accuracy.medium negativeResult -positiveResult "arsinh should be an odd function"
    ]

[<Tests>]
let roundToTests =
    testList "Ops.roundTo" [
        testCase "roundTo 0 decimals" <| fun () ->
            let result = Ops.roundTo 0 3.7
            Expect.floatClose Accuracy.high result 4.0 "3.7 rounded to 0 decimals should be 4"
        
        testCase "roundTo 1 decimal" <| fun () ->
            let result = Ops.roundTo 1 3.14159
            Expect.floatClose Accuracy.high result 3.1 "3.14159 rounded to 1 decimal should be 3.1"
        
        testCase "roundTo 2 decimals" <| fun () ->
            let result = Ops.roundTo 2 3.14159
            Expect.floatClose Accuracy.high result 3.14 "3.14159 rounded to 2 decimals should be 3.14"
        
        testCase "roundTo 3 decimals" <| fun () ->
            let result = Ops.roundTo 3 3.14159
            Expect.floatClose Accuracy.high result 3.142 "3.14159 rounded to 3 decimals should be 3.142"
        
        testCase "roundTo negative number" <| fun () ->
            let result = Ops.roundTo 2 -5.678
            Expect.floatClose Accuracy.high result -5.68 "-5.678 rounded to 2 decimals should be -5.68"
        
        testCase "roundTo zero" <| fun () ->
            let result = Ops.roundTo 2 0.0
            Expect.floatClose Accuracy.high result 0.0 "0 rounded to 2 decimals should be 0"
    ]

[<Tests>]
let signumTests =
    testList "Ops.signum" [
        testCase "signum with both positive" <| fun () ->
            let result = Ops.signum 5.0 3.0
            Expect.floatClose Accuracy.high result 5.0 "signum(5, 3) should be 5"
        
        testCase "signum with positive a and negative b" <| fun () ->
            let result = Ops.signum 5.0 -3.0
            Expect.floatClose Accuracy.high result -5.0 "signum(5, -3) should be -5"
        
        testCase "signum with negative a and positive b" <| fun () ->
            let result = Ops.signum -5.0 3.0
            Expect.floatClose Accuracy.high result 5.0 "signum(-5, 3) should be 5"
        
        testCase "signum with both negative" <| fun () ->
            let result = Ops.signum -5.0 -3.0
            Expect.floatClose Accuracy.high result -5.0 "signum(-5, -3) should be -5"
        
        testCase "signum with zero a and positive b" <| fun () ->
            let result = Ops.signum 0.0 5.0
            Expect.floatClose Accuracy.high result 0.0 "signum(0, 5) should be 0"
        
        testCase "signum with zero a and negative b" <| fun () ->
            let result = Ops.signum 0.0 -5.0
            Expect.floatClose Accuracy.high result 0.0 "signum(0, -5) should be 0"
        
        testCase "signum with positive a and zero b" <| fun () ->
            let result = Ops.signum 5.0 0.0
            Expect.floatClose Accuracy.high result 5.0 "signum(5, 0) should be 5"
        
        testCase "signum with negative a and zero b" <| fun () ->
            let result = Ops.signum -5.0 0.0
            Expect.floatClose Accuracy.high result 5.0 "signum(-5, 0) should be 5"
    ]
