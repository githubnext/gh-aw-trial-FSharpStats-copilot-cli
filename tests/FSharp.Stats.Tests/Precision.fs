module PrecisionTests

open Expecto
open FSharp.Stats

[<Tests>]
let almostEqualNormRelativeTests =
    testList
        "Precision.almostEqualNormRelative"
        [ testCase "Equal values within tolerance"
          <| fun () ->
              let a = 1.0
              let b = 1.0
              let result = Precision.almostEqualNormRelative 0.001 a b
              Expect.isTrue result "Equal values should be almost equal"

          testCase "Values within tolerance"
          <| fun () ->
              let a = 1.0
              let b = 1.00000001
              let result = Precision.almostEqualNormRelative 0.001 a b
              Expect.isTrue result "Values within tolerance should be almost equal"

          testCase "Values outside tolerance"
          <| fun () ->
              let a = 1.0
              let b = 1.1
              let result = Precision.almostEqualNormRelative 0.001 a b
              Expect.isFalse result "Values outside tolerance should not be almost equal"

          testCase "Zero values within tolerance"
          <| fun () ->
              let a = 0.0
              let b = 0.0
              let result = Precision.almostEqualNormRelative 0.001 a b
              Expect.isTrue result "Zero values should be almost equal"

          testCase "Small difference near zero"
          <| fun () ->
              let a = 0.0
              let b = 0.0000001
              let result = Precision.almostEqualNormRelative 0.001 a b
              Expect.isTrue result "Small difference near zero should be within tolerance"

          testCase "Negative values within tolerance"
          <| fun () ->
              let a = -1.0
              let b = -1.00000001
              let result = Precision.almostEqualNormRelative 0.001 a b
              Expect.isTrue result "Negative values within tolerance should be almost equal"

          testCase "Negative values outside tolerance"
          <| fun () ->
              let a = -1.0
              let b = -1.1
              let result = Precision.almostEqualNormRelative 0.001 a b
              Expect.isFalse result "Negative values outside tolerance should not be almost equal"

          testCase "Positive and negative values"
          <| fun () ->
              let a = 1.0
              let b = -1.0
              let result = Precision.almostEqualNormRelative 0.001 a b
              Expect.isFalse result "Positive and negative values should not be almost equal"

          testCase "Both positive infinity"
          <| fun () ->
              let a = infinity
              let b = infinity
              let result = Precision.almostEqualNormRelative 0.001 a b
              Expect.isTrue result "Both positive infinity should be equal"

          testCase "Both negative infinity"
          <| fun () ->
              let a = -infinity
              let b = -infinity
              let result = Precision.almostEqualNormRelative 0.001 a b
              Expect.isTrue result "Both negative infinity should be equal"

          testCase "Positive and negative infinity"
          <| fun () ->
              let a = infinity
              let b = -infinity
              let result = Precision.almostEqualNormRelative 0.001 a b
              Expect.isFalse result "Positive and negative infinity should not be equal"

          testCase "Infinity and finite value"
          <| fun () ->
              let a = infinity
              let b = 1.0
              let result = Precision.almostEqualNormRelative 0.001 a b
              Expect.isFalse result "Infinity and finite value should not be almost equal"

          testCase "Both NaN"
          <| fun () ->
              let a = nan
              let b = nan
              let result = Precision.almostEqualNormRelative 0.001 a b
              Expect.isFalse result "NaN values should never be almost equal (IEEE 754)"

          testCase "NaN and finite value"
          <| fun () ->
              let a = nan
              let b = 1.0
              let result = Precision.almostEqualNormRelative 0.001 a b
              Expect.isFalse result "NaN and finite value should not be almost equal"

          testCase "Finite value and NaN"
          <| fun () ->
              let a = 1.0
              let b = nan
              let result = Precision.almostEqualNormRelative 0.001 a b
              Expect.isFalse result "Finite value and NaN should not be almost equal"

          testCase "Large values within tolerance"
          <| fun () ->
              let a = 1000000.0
              let b = 1000000.0001
              let result = Precision.almostEqualNormRelative 0.001 a b
              Expect.isTrue result "Large values within tolerance should be almost equal"

          testCase "Large values outside tolerance"
          <| fun () ->
              let a = 1000000.0
              let b = 1000001.0
              let result = Precision.almostEqualNormRelative 0.001 a b
              Expect.isFalse result "Large values outside tolerance should not be almost equal"

          testCase "Very small tolerance"
          <| fun () ->
              let a = 1.0
              let b = 1.0 + 1e-10
              let result = Precision.almostEqualNormRelative 1e-9 a b
              Expect.isTrue result "Values should be almost equal with very small tolerance"

          testCase "Very large tolerance"
          <| fun () ->
              let a = 1.0
              let b = 2.0
              let result = Precision.almostEqualNormRelative 10.0 a b
              Expect.isTrue result "Values should be almost equal with very large tolerance"

          testCase "Zero tolerance almost zero difference"
          <| fun () ->
              let a = 1.0
              let b = 1.0 + 1e-100 // Tiny difference but still > 0
              let result = Precision.almostEqualNormRelative 1e-99 a b
              Expect.isTrue result "Very tiny difference should be within tolerance"

          testCase "Zero tolerance no match"
          <| fun () ->
              let a = 1.0
              let b = 1.0 + 1e-100 // Any difference with zero tolerance
              let result = Precision.almostEqualNormRelative 0.0 a b
              Expect.isFalse result "Any difference with zero tolerance should not be almost equal" ]

[<Tests>]
let almostEqualNormTests =
    testList
        "Precision.almostEqualNorm"
        [ testCase "Equal values"
          <| fun () ->
              let a = 1.0
              let b = 1.0
              let result = Precision.almostEqualNorm a b
              Expect.isTrue result "Equal values should be almost equal"

          testCase "Values within default tolerance"
          <| fun () ->
              let a = 1.0
              let b = 1.0 + 1e-15 // Well within 2.22e-15
              let result = Precision.almostEqualNorm a b
              Expect.isTrue result "Values within default tolerance should be almost equal"

          testCase "Values outside default tolerance"
          <| fun () ->
              let a = 1.0
              let b = 1.0 + 1e-10 // Outside 10 * 2^-52
              let result = Precision.almostEqualNorm a b
              Expect.isFalse result "Values outside default tolerance should not be almost equal"

          testCase "Zero values"
          <| fun () ->
              let a = 0.0
              let b = 0.0
              let result = Precision.almostEqualNorm a b
              Expect.isTrue result "Zero values should be almost equal"

          testCase "Negative values within tolerance"
          <| fun () ->
              let a = -1.0
              let b = -1.0 - 1e-15
              let result = Precision.almostEqualNorm a b
              Expect.isTrue result "Negative values within tolerance should be almost equal"

          testCase "Both positive infinity"
          <| fun () ->
              let a = infinity
              let b = infinity
              let result = Precision.almostEqualNorm a b
              Expect.isTrue result "Both positive infinity should be equal"

          testCase "Both negative infinity"
          <| fun () ->
              let a = -infinity
              let b = -infinity
              let result = Precision.almostEqualNorm a b
              Expect.isTrue result "Both negative infinity should be equal"

          testCase "Different infinities"
          <| fun () ->
              let a = infinity
              let b = -infinity
              let result = Precision.almostEqualNorm a b
              Expect.isFalse result "Different infinities should not be equal"

          testCase "Both NaN"
          <| fun () ->
              let a = nan
              let b = nan
              let result = Precision.almostEqualNorm a b
              Expect.isFalse result "NaN values should never be almost equal"

          testCase "NaN and finite value"
          <| fun () ->
              let a = nan
              let b = 1.0
              let result = Precision.almostEqualNorm a b
              Expect.isFalse result "NaN and finite value should not be almost equal"

          testCase "Very small numbers within tolerance"
          <| fun () ->
              let a = 1e-300
              let b = 1e-300 + 1e-314
              let result = Precision.almostEqualNorm a b
              Expect.isTrue result "Very small numbers within tolerance should be almost equal"

          testCase "Very large numbers within tolerance"
          <| fun () ->
              let a = 1.0
              let b = 1.0 + 1e-16 // Within 2.22e-15
              let result = Precision.almostEqualNorm a b
              Expect.isTrue result "Values within tolerance should be almost equal (absolute comparison)"

          testCase "Edge case near zero"
          <| fun () ->
              let a = 0.0
              let b = 1e-15 // Just at the edge of tolerance
              let result = Precision.almostEqualNorm a b
              Expect.isTrue result "Values at the edge of tolerance near zero should be almost equal"

          testCase "Commutative property"
          <| fun () ->
              let a = 1.0
              let b = 1.0 + 1e-14
              let result1 = Precision.almostEqualNorm a b
              let result2 = Precision.almostEqualNorm b a
              Expect.equal result1 result2 "almostEqualNorm should be commutative"

          testCase "Different signs close to zero"
          <| fun () ->
              let a = 1e-16
              let b = -1e-16
              let result = Precision.almostEqualNorm a b
              Expect.isTrue result "Very small values of different signs should be almost equal" ]
