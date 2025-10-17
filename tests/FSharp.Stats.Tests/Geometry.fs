module GeometryTests

open Expecto
open System
open FSharp.Stats

[<Tests>]
let geometryTests =
    testList
        "Geometry"
        [
          // Basic hypot tests
          testCase "hypot of 3 and 4 equals 5"
          <| fun () ->
              let result = Geometry.hypot 3.0 4.0
              Expect.floatClose Accuracy.high result 5.0 "hypot(3,4) should equal 5"

          testCase "hypot of 5 and 12 equals 13"
          <| fun () ->
              let result = Geometry.hypot 5.0 12.0
              Expect.floatClose Accuracy.high result 13.0 "hypot(5,12) should equal 13"

          testCase "hypot of 8 and 15 equals 17"
          <| fun () ->
              let result = Geometry.hypot 8.0 15.0
              Expect.floatClose Accuracy.high result 17.0 "hypot(8,15) should equal 17"

          // Edge cases with zero
          testCase "hypot with zero first argument"
          <| fun () ->
              let result = Geometry.hypot 0.0 5.0
              Expect.floatClose Accuracy.high result 5.0 "hypot(0,5) should equal 5"

          testCase "hypot with zero second argument"
          <| fun () ->
              let result = Geometry.hypot 5.0 0.0
              Expect.floatClose Accuracy.high result 5.0 "hypot(5,0) should equal 5"

          testCase "hypot with both zeros"
          <| fun () ->
              let result = Geometry.hypot 0.0 0.0
              Expect.floatClose Accuracy.high result 0.0 "hypot(0,0) should equal 0"

          // Negative values
          testCase "hypot with negative first argument"
          <| fun () ->
              let result = Geometry.hypot -3.0 4.0
              Expect.floatClose Accuracy.high result 5.0 "hypot(-3,4) should equal 5"

          testCase "hypot with negative second argument"
          <| fun () ->
              let result = Geometry.hypot 3.0 -4.0
              Expect.floatClose Accuracy.high result 5.0 "hypot(3,-4) should equal 5"

          testCase "hypot with both negative arguments"
          <| fun () ->
              let result = Geometry.hypot -3.0 -4.0
              Expect.floatClose Accuracy.high result 5.0 "hypot(-3,-4) should equal 5"

          // Commutative property
          testCase "hypot is commutative"
          <| fun () ->
              let result1 = Geometry.hypot 7.0 24.0
              let result2 = Geometry.hypot 24.0 7.0
              Expect.floatClose Accuracy.high result1 result2 "hypot should be commutative"

          // Very small values
          testCase "hypot with very small values"
          <| fun () ->
              let result = Geometry.hypot 0.0001 0.0001
              let expected = sqrt (0.0001 * 0.0001 + 0.0001 * 0.0001)
              Expect.floatClose Accuracy.high result expected "hypot should handle very small values"

          // Very large values (testing overflow protection)
          testCase "hypot with large values"
          <| fun () ->
              let a = 1e150
              let b = 1e150
              let result = Geometry.hypot a b
              let expected = sqrt 2.0 * 1e150
              Expect.floatClose Accuracy.low result expected "hypot should handle large values without overflow"

          // One value much larger than the other
          testCase "hypot when first value much larger"
          <| fun () ->
              let result = Geometry.hypot 1e10 1.0
              let expected = 1e10
              Expect.floatClose Accuracy.high result expected "hypot should handle first value much larger"

          testCase "hypot when second value much larger"
          <| fun () ->
              let result = Geometry.hypot 1.0 1e10
              let expected = 1e10
              Expect.floatClose Accuracy.high result expected "hypot should handle second value much larger"

          // Unit values
          testCase "hypot of 1 and 1"
          <| fun () ->
              let result = Geometry.hypot 1.0 1.0
              let expected = sqrt 2.0
              Expect.floatClose Accuracy.high result expected "hypot(1,1) should equal sqrt(2)"

          // Non-integer Pythagorean-like values
          testCase "hypot with decimal values"
          <| fun () ->
              let result = Geometry.hypot 1.5 2.0
              let expected = 2.5
              Expect.floatClose Accuracy.high result expected "hypot(1.5,2.0) should equal 2.5"

          // Comparison with naive sqrt implementation
          testCase "hypot matches sqrt(a^2 + b^2) for moderate values"
          <| fun () ->
              let a = 6.0
              let b = 8.0
              let result = Geometry.hypot a b
              let naive = sqrt (a * a + b * b)
              Expect.floatClose Accuracy.high result naive "hypot should match naive calculation for moderate values"

          // Test special case: equal values
          testCase "hypot with equal positive values"
          <| fun () ->
              let result = Geometry.hypot 7.0 7.0
              let expected = 7.0 * sqrt 2.0
              Expect.floatClose Accuracy.high result expected "hypot with equal values"

          // Fractional values
          testCase "hypot with fractional values"
          <| fun () ->
              let result = Geometry.hypot 0.6 0.8
              Expect.floatClose Accuracy.high result 1.0 "hypot(0.6,0.8) should equal 1.0"

          // Very different magnitudes
          testCase "hypot with very different magnitudes"
          <| fun () ->
              let result = Geometry.hypot 1e-100 1e100
              let expected = 1e100
              Expect.floatClose Accuracy.low result expected "hypot should handle very different magnitudes" ]
