module BigRationalTests

open Expecto
open System
open System.Numerics
open FSharp.Stats

[<Tests>]
let bigRationalTests =
    testList
        "BigRational"
        [
          // Basic construction and properties
          testCase "FromInt creates integer BigRational"
          <| fun () ->
              let br = BigRational.FromInt 5
              Expect.equal (BigRational.ToInt32 br) 5 "FromInt should create integer BigRational"

          testCase "FromBigInt creates BigRational from BigInteger"
          <| fun () ->
              let bi = BigInteger(42)
              let br = BigRational.FromBigInt bi
              Expect.equal (BigRational.ToBigInt br) bi "FromBigInt should create BigRational"

          testCase "Zero property"
          <| fun () ->
              let zero = BigRational.Zero
              Expect.equal (BigRational.ToInt32 zero) 0 "Zero should be 0"

          testCase "One property"
          <| fun () ->
              let one = BigRational.One
              Expect.equal (BigRational.ToInt32 one) 1 "One should be 1"

          testCase "Parse integer string"
          <| fun () ->
              let br = BigRational.Parse "42"
              Expect.equal (BigRational.ToInt32 br) 42 "Parse should handle integer strings"

          testCase "Parse rational string"
          <| fun () ->
              let br = BigRational.Parse "3/4"
              let expected = 0.75
              Expect.floatClose Accuracy.high (BigRational.ToDouble br) expected "Parse should handle rational strings"

          testCase "Parse negative rational"
          <| fun () ->
              let br = BigRational.Parse "-5/2"
              let expected = -2.5
              Expect.floatClose
                  Accuracy.high
                  (BigRational.ToDouble br)
                  expected
                  "Parse should handle negative rationals"

          // Addition tests
          testCase "Add two integers"
          <| fun () ->
              let br1 = BigRational.FromInt 3
              let br2 = BigRational.FromInt 4
              let result = br1 + br2
              Expect.equal (BigRational.ToInt32 result) 7 "3 + 4 should equal 7"

          testCase "Add integer and rational"
          <| fun () ->
              let br1 = BigRational.FromInt 2
              let br2 = BigRational.Parse "1/2"
              let result = br1 + br2
              Expect.floatClose Accuracy.high (BigRational.ToDouble result) 2.5 "2 + 1/2 should equal 2.5"

          testCase "Add two rationals"
          <| fun () ->
              let br1 = BigRational.Parse "1/3"
              let br2 = BigRational.Parse "1/6"
              let result = br1 + br2
              Expect.floatClose Accuracy.high (BigRational.ToDouble result) 0.5 "1/3 + 1/6 should equal 1/2"

          testCase "Add with zero"
          <| fun () ->
              let br = BigRational.FromInt 5
              let result = br + BigRational.Zero
              Expect.equal (BigRational.ToInt32 result) 5 "Adding zero should not change value"

          // Subtraction tests
          testCase "Subtract two integers"
          <| fun () ->
              let br1 = BigRational.FromInt 10
              let br2 = BigRational.FromInt 3
              let result = br1 - br2
              Expect.equal (BigRational.ToInt32 result) 7 "10 - 3 should equal 7"

          testCase "Subtract resulting in negative"
          <| fun () ->
              let br1 = BigRational.FromInt 3
              let br2 = BigRational.FromInt 5
              let result = br1 - br2
              Expect.equal (BigRational.ToInt32 result) -2 "3 - 5 should equal -2"

          testCase "Subtract rationals"
          <| fun () ->
              let br1 = BigRational.Parse "3/4"
              let br2 = BigRational.Parse "1/4"
              let result = br1 - br2
              Expect.floatClose Accuracy.high (BigRational.ToDouble result) 0.5 "3/4 - 1/4 should equal 1/2"

          // Multiplication tests
          testCase "Multiply two integers"
          <| fun () ->
              let br1 = BigRational.FromInt 6
              let br2 = BigRational.FromInt 7
              let result = br1 * br2
              Expect.equal (BigRational.ToInt32 result) 42 "6 * 7 should equal 42"

          testCase "Multiply by zero"
          <| fun () ->
              let br = BigRational.FromInt 5
              let result = br * BigRational.Zero
              Expect.equal (BigRational.ToInt32 result) 0 "Multiplying by zero should give zero"

          testCase "Multiply rationals"
          <| fun () ->
              let br1 = BigRational.Parse "2/3"
              let br2 = BigRational.Parse "3/4"
              let result = br1 * br2
              Expect.floatClose Accuracy.high (BigRational.ToDouble result) 0.5 "2/3 * 3/4 should equal 1/2"

          testCase "Multiply integer by rational"
          <| fun () ->
              let br1 = BigRational.FromInt 3
              let br2 = BigRational.Parse "1/2"
              let result = br1 * br2
              Expect.floatClose Accuracy.high (BigRational.ToDouble result) 1.5 "3 * 1/2 should equal 1.5"

          // Division tests
          testCase "Divide two integers"
          <| fun () ->
              let br1 = BigRational.FromInt 10
              let br2 = BigRational.FromInt 4
              let result = br1 / br2
              Expect.floatClose Accuracy.high (BigRational.ToDouble result) 2.5 "10 / 4 should equal 2.5"

          testCase "Divide rationals"
          <| fun () ->
              let br1 = BigRational.Parse "3/4"
              let br2 = BigRational.Parse "1/2"
              let result = br1 / br2
              Expect.floatClose Accuracy.high (BigRational.ToDouble result) 1.5 "3/4 / 1/2 should equal 1.5"

          testCase "Divide by one"
          <| fun () ->
              let br = BigRational.FromInt 42
              let result = br / BigRational.One
              Expect.equal (BigRational.ToInt32 result) 42 "Dividing by one should not change value"

          // Negation tests
          testCase "Negate positive integer"
          <| fun () ->
              let br = BigRational.FromInt 5
              let result = -br
              Expect.equal (BigRational.ToInt32 result) -5 "Negating 5 should give -5"

          testCase "Negate negative integer"
          <| fun () ->
              let br = BigRational.FromInt -3
              let result = -br
              Expect.equal (BigRational.ToInt32 result) 3 "Negating -3 should give 3"

          testCase "Negate rational"
          <| fun () ->
              let br = BigRational.Parse "3/4"
              let result = -br
              Expect.floatClose Accuracy.high (BigRational.ToDouble result) -0.75 "Negating 3/4 should give -3/4"

          // Comparison tests
          testCase "Equality of same integers"
          <| fun () ->
              let br1 = BigRational.FromInt 5
              let br2 = BigRational.FromInt 5
              Expect.equal br1 br2 "Equal BigRationals should be equal"

          testCase "Inequality of different integers"
          <| fun () ->
              let br1 = BigRational.FromInt 5
              let br2 = BigRational.FromInt 3
              Expect.notEqual br1 br2 "Different BigRationals should not be equal"

          testCase "Equality of equivalent rationals"
          <| fun () ->
              let br1 = BigRational.Parse "2/4"
              let br2 = BigRational.Parse "1/2"
              Expect.equal br1 br2 "Equivalent fractions should be equal"

          testCase "Less than comparison"
          <| fun () ->
              let br1 = BigRational.FromInt 3
              let br2 = BigRational.FromInt 5
              Expect.isTrue (br1 < br2) "3 should be less than 5"

          testCase "Greater than comparison"
          <| fun () ->
              let br1 = BigRational.FromInt 7
              let br2 = BigRational.FromInt 4
              Expect.isTrue (br1 > br2) "7 should be greater than 4"

          testCase "Less than or equal"
          <| fun () ->
              let br1 = BigRational.FromInt 3
              let br2 = BigRational.FromInt 3
              Expect.isTrue (br1 <= br2) "3 should be less than or equal to 3"

          testCase "Greater than or equal"
          <| fun () ->
              let br1 = BigRational.FromInt 5
              let br2 = BigRational.FromInt 5
              Expect.isTrue (br1 >= br2) "5 should be greater than or equal to 5"

          // Property tests
          testCase "IsNegative for negative number"
          <| fun () ->
              let br = BigRational.FromInt -5
              Expect.isTrue br.IsNegative "Negative number should have IsNegative true"

          testCase "IsNegative for positive number"
          <| fun () ->
              let br = BigRational.FromInt 5
              Expect.isFalse br.IsNegative "Positive number should have IsNegative false"

          testCase "IsPositive for positive number"
          <| fun () ->
              let br = BigRational.FromInt 5
              Expect.isTrue br.IsPositive "Positive number should have IsPositive true"

          testCase "IsPositive for negative number"
          <| fun () ->
              let br = BigRational.FromInt -5
              Expect.isFalse br.IsPositive "Negative number should have IsPositive false"

          testCase "Sign of positive number"
          <| fun () ->
              let br = BigRational.FromInt 5
              Expect.equal br.Sign 1 "Sign of positive number should be 1"

          testCase "Sign of negative number"
          <| fun () ->
              let br = BigRational.FromInt -5
              Expect.equal br.Sign -1 "Sign of negative number should be -1"

          testCase "Sign of zero"
          <| fun () ->
              let br = BigRational.Zero
              Expect.equal br.Sign 0 "Sign of zero should be 0"

          testCase "Numerator and denominator of rational"
          <| fun () ->
              let br = BigRational.Parse "3/4"
              Expect.equal br.Numerator (BigInteger 3) "Numerator should be 3"
              Expect.equal br.Denominator (BigInteger 4) "Denominator should be 4"

          testCase "Numerator and denominator of integer"
          <| fun () ->
              let br = BigRational.FromInt 5
              Expect.equal br.Numerator (BigInteger 5) "Numerator should be 5"
              Expect.equal br.Denominator (BigInteger 1) "Denominator should be 1"

          // Power tests
          testCase "Power of integer"
          <| fun () ->
              let br = BigRational.FromInt 2
              let result = BigRational.PowN(br, 3)
              Expect.equal (BigRational.ToInt32 result) 8 "2^3 should equal 8"

          testCase "Power of rational"
          <| fun () ->
              let br = BigRational.Parse "1/2"
              let result = BigRational.PowN(br, 2)
              Expect.floatClose Accuracy.high (BigRational.ToDouble result) 0.25 "(1/2)^2 should equal 1/4"

          testCase "Power to zero"
          <| fun () ->
              let br = BigRational.FromInt 5
              let result = BigRational.PowN(br, 0)
              Expect.equal (BigRational.ToInt32 result) 1 "Any number to power 0 should equal 1"

          // Abs tests
          testCase "Abs of positive number"
          <| fun () ->
              let br = BigRational.FromInt 5
              let result = BigRational.Abs(br)
              Expect.equal (BigRational.ToInt32 result) 5 "Abs of positive should be unchanged"

          testCase "Abs of negative number"
          <| fun () ->
              let br = BigRational.FromInt -5
              let result = BigRational.Abs(br)
              Expect.equal (BigRational.ToInt32 result) 5 "Abs of negative should be positive"

          testCase "Abs of zero"
          <| fun () ->
              let br = BigRational.Zero
              let result = BigRational.Abs(br)
              Expect.equal (BigRational.ToInt32 result) 0 "Abs of zero should be zero"

          // Conversion tests
          testCase "ToDouble conversion"
          <| fun () ->
              let br = BigRational.Parse "7/4"
              let result = BigRational.ToDouble br
              Expect.floatClose Accuracy.high result 1.75 "7/4 should convert to 1.75"

          testCase "ToBigInt conversion"
          <| fun () ->
              let br = BigRational.Parse "9/2"
              let result = BigRational.ToBigInt br
              Expect.equal result (BigInteger 4) "9/2 should truncate to 4"

          testCase "ToInt32 conversion"
          <| fun () ->
              let br = BigRational.Parse "11/3"
              let result = BigRational.ToInt32 br
              Expect.equal result 3 "11/3 should truncate to 3"

          // ToString tests
          testCase "ToString of integer"
          <| fun () ->
              let br = BigRational.FromInt 42
              let result = br.ToString()
              Expect.equal result "42" "ToString of integer should be simple"

          testCase "ToString of rational"
          <| fun () ->
              let br = BigRational.Parse "3/4"
              let result = br.ToString()
              Expect.equal result "3/4" "ToString of rational should show fraction"

          // Edge cases
          testCase "Addition is commutative"
          <| fun () ->
              let br1 = BigRational.Parse "2/3"
              let br2 = BigRational.Parse "3/4"
              Expect.equal (br1 + br2) (br2 + br1) "Addition should be commutative"

          testCase "Multiplication is commutative"
          <| fun () ->
              let br1 = BigRational.Parse "2/3"
              let br2 = BigRational.Parse "3/4"
              Expect.equal (br1 * br2) (br2 * br1) "Multiplication should be commutative"

          testCase "Subtraction of self equals zero"
          <| fun () ->
              let br = BigRational.Parse "7/3"
              let result = br - br
              Expect.equal result BigRational.Zero "Subtracting self should give zero"

          testCase "Division of self equals one"
          <| fun () ->
              let br = BigRational.Parse "7/3"
              let result = br / br
              Expect.equal result BigRational.One "Dividing self should give one" ]
