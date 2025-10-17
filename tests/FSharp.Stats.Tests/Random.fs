module Random

open Expecto
open FSharp.Stats

[<Tests>]
let randomTests =
    testList
        "Random"
        [ testList
              "RandBasic"
              [ testCase "RandBasic default constructor creates instance"
                <| fun () ->
                    let rng = Random.RandBasic()
                    Expect.isNotNull (box rng) "RandBasic should be created"

                testCase "RandBasic with seed creates instance"
                <| fun () ->
                    let rng = Random.RandBasic(42)
                    Expect.isNotNull (box rng) "RandBasic with seed should be created"

                testCase "RandBasic with negative seed creates instance"
                <| fun () ->
                    let rng = Random.RandBasic(-1)
                    Expect.isNotNull (box rng) "RandBasic with negative seed should be created"

                testCase "RandBasic NextInt returns non-negative integer"
                <| fun () ->
                    let rng = Random.RandBasic(42) :> Random.IRandom
                    let value = rng.NextInt()
                    Expect.isGreaterThanOrEqual value 0 "NextInt should return non-negative value"

                testCase "RandBasic NextInt with maxValue returns value less than max"
                <| fun () ->
                    let rng = Random.RandBasic(42) :> Random.IRandom
                    let maxValue = 100
                    let value = rng.NextInt(maxValue)
                    Expect.isLessThan value maxValue "NextInt should return value less than maxValue"
                    Expect.isGreaterThanOrEqual value 0 "NextInt should return non-negative value"

                testCase "RandBasic NextInt with maxValue 0 returns 0"
                <| fun () ->
                    let rng = Random.RandBasic(42) :> Random.IRandom
                    let value = rng.NextInt(0)
                    Expect.equal value 0 "NextInt with maxValue 0 should return 0"

                testCase "RandBasic NextFloat returns value between 0 and 1"
                <| fun () ->
                    let rng = Random.RandBasic(42) :> Random.IRandom
                    let value = rng.NextFloat()
                    Expect.isGreaterThanOrEqual value 0.0 "NextFloat should return value >= 0"
                    Expect.isLessThan value 1.0 "NextFloat should return value < 1"

                testCase "RandBasic with same seed produces same sequence"
                <| fun () ->
                    let rng1 = Random.RandBasic(42) :> Random.IRandom
                    let rng2 = Random.RandBasic(42) :> Random.IRandom
                    let values1 = [ for _ in 1..10 -> rng1.NextInt() ]
                    let values2 = [ for _ in 1..10 -> rng2.NextInt() ]
                    Expect.equal values1 values2 "Same seed should produce same sequence" ]

          testList
              "RandThreadSafe"
              [ testCase "RandThreadSafe default constructor creates instance"
                <| fun () ->
                    let rng = Random.RandThreadSafe()
                    Expect.isNotNull (box rng) "RandThreadSafe should be created"

                testCase "RandThreadSafe with seed creates instance"
                <| fun () ->
                    let rng = Random.RandThreadSafe(42)
                    Expect.isNotNull (box rng) "RandThreadSafe with seed should be created"

                testCase "RandThreadSafe with negative seed creates instance"
                <| fun () ->
                    let rng = Random.RandThreadSafe(-1)
                    Expect.isNotNull (box rng) "RandThreadSafe with negative seed should be created"

                testCase "RandThreadSafe NextInt returns non-negative integer"
                <| fun () ->
                    let rng = Random.RandThreadSafe(42) :> Random.IRandom
                    let value = rng.NextInt()
                    Expect.isGreaterThanOrEqual value 0 "NextInt should return non-negative value"

                testCase "RandThreadSafe NextInt with maxValue returns value less than max"
                <| fun () ->
                    let rng = Random.RandThreadSafe(42) :> Random.IRandom
                    let maxValue = 100
                    let value = rng.NextInt(maxValue)
                    Expect.isLessThan value maxValue "NextInt should return value less than maxValue"
                    Expect.isGreaterThanOrEqual value 0 "NextInt should return non-negative value"

                testCase "RandThreadSafe NextInt with maxValue 0 returns 0"
                <| fun () ->
                    let rng = Random.RandThreadSafe(42) :> Random.IRandom
                    let value = rng.NextInt(0)
                    Expect.equal value 0 "NextInt with maxValue 0 should return 0"

                testCase "RandThreadSafe NextFloat returns value between 0 and 1"
                <| fun () ->
                    let rng = Random.RandThreadSafe(42) :> Random.IRandom
                    let value = rng.NextFloat()
                    Expect.isGreaterThanOrEqual value 0.0 "NextFloat should return value >= 0"
                    Expect.isLessThan value 1.0 "NextFloat should return value < 1"

                testCase "RandThreadSafe with same seed produces same sequence"
                <| fun () ->
                    let rng1 = Random.RandThreadSafe(42) :> Random.IRandom
                    let rng2 = Random.RandThreadSafe(42) :> Random.IRandom
                    let values1 = [ for _ in 1..10 -> rng1.NextInt() ]
                    let values2 = [ for _ in 1..10 -> rng2.NextInt() ]
                    Expect.equal values1 values2 "Same seed should produce same sequence" ]

          testList
              "SampleGenerator"
              [ testCase "GetSampleGenerator returns default generator"
                <| fun () ->
                    let gen = Random.GetSampleGenerator()
                    Expect.isNotNull (box gen) "GetSampleGenerator should return a generator"

                testCase "SetSampleGenerator changes the generator"
                <| fun () ->
                    let original = Random.GetSampleGenerator()
                    let newGen = Random.RandBasic(123) :> Random.IRandom
                    Random.SetSampleGenerator newGen
                    let current = Random.GetSampleGenerator()
                    Expect.equal current newGen "SetSampleGenerator should change the generator"
                    // Restore original
                    Random.SetSampleGenerator original

                testCase "SetSampleGenerator affects subsequent calls"
                <| fun () ->
                    let original = Random.GetSampleGenerator()
                    let seededGen = Random.RandBasic(42) :> Random.IRandom
                    Random.SetSampleGenerator seededGen

                    // Generate some values using the set generator
                    let value1 = Random.GetSampleGenerator().NextInt()

                    // Reset and create another with same seed
                    let seededGen2 = Random.RandBasic(42) :> Random.IRandom
                    Random.SetSampleGenerator seededGen2
                    let value2 = Random.GetSampleGenerator().NextInt()

                    Expect.equal value1 value2 "Same seed should produce same first value"

                    // Restore original
                    Random.SetSampleGenerator original ]

          testList
              "boxMullerTransform"
              [ testCase "boxMullerTransform returns two values"
                <| fun () ->
                    let original = Random.GetSampleGenerator()
                    Random.SetSampleGenerator(Random.RandBasic(42) :> Random.IRandom)

                    let (z0, z1) = Random.boxMullerTransform ()
                    Expect.isNotNaN z0 "First value should not be NaN"
                    Expect.isNotNaN z1 "Second value should not be NaN"
                    Expect.isNotInfinity z0 "First value should not be infinity"
                    Expect.isNotInfinity z1 "Second value should not be infinity"

                    Random.SetSampleGenerator original

                testCase "boxMullerTransform produces different values"
                <| fun () ->
                    let original = Random.GetSampleGenerator()
                    Random.SetSampleGenerator(Random.RandBasic(42) :> Random.IRandom)

                    let (z0, z1) = Random.boxMullerTransform ()
                    Expect.notEqual z0 z1 "Two values should be different"

                    Random.SetSampleGenerator original

                testCase "boxMullerTransform with same seed produces same results"
                <| fun () ->
                    let original = Random.GetSampleGenerator()

                    Random.SetSampleGenerator(Random.RandBasic(42) :> Random.IRandom)
                    let (z0_1, z1_1) = Random.boxMullerTransform ()

                    Random.SetSampleGenerator(Random.RandBasic(42) :> Random.IRandom)
                    let (z0_2, z1_2) = Random.boxMullerTransform ()

                    Expect.floatClose Accuracy.high z0_1 z0_2 "Same seed should produce same first value"
                    Expect.floatClose Accuracy.high z1_1 z1_2 "Same seed should produce same second value"

                    Random.SetSampleGenerator original

                testCase "boxMullerTransform values are approximately normally distributed"
                <| fun () ->
                    let original = Random.GetSampleGenerator()
                    Random.SetSampleGenerator(Random.RandBasic(42) :> Random.IRandom)

                    // Generate many values
                    let values =
                        [ for _ in 1..1000 ->
                              let (z0, z1) = Random.boxMullerTransform ()
                              [ z0
                                z1 ] ]
                        |> List.concat

                    // Calculate mean (should be close to 0 for standard normal)
                    let mean = values |> List.average
                    // With 2000 samples, we expect the mean to be within about 0.1 of 0
                    Expect.isLessThan (abs mean) 0.1 "Mean should be close to 0 (within 0.1)"

                    // Calculate variance (should be close to 1 for standard normal)
                    let variance = values |> List.map (fun x -> (x - mean) ** 2.0) |> List.average
                    // With 2000 samples, variance should be within about 0.15 of 1
                    Expect.isLessThan (abs (variance - 1.0)) 0.15 "Variance should be close to 1 (within 0.15)"

                    Random.SetSampleGenerator original

                testCase "boxMullerTransform multiple calls produce different values"
                <| fun () ->
                    let original = Random.GetSampleGenerator()
                    Random.SetSampleGenerator(Random.RandBasic(42) :> Random.IRandom)

                    let (z0_1, z1_1) = Random.boxMullerTransform ()
                    let (z0_2, z1_2) = Random.boxMullerTransform ()

                    Expect.notEqual z0_1 z0_2 "First values from different calls should differ"
                    Expect.notEqual z1_1 z1_2 "Second values from different calls should differ"

                    Random.SetSampleGenerator original ]

          testList
              "IRandom interface"
              [ testCase "IRandom interface can be implemented by RandBasic"
                <| fun () ->
                    let rng: Random.IRandom = Random.RandBasic(42) :> Random.IRandom
                    let _ = rng.NextInt()
                    let _ = rng.NextInt(100)
                    let _ = rng.NextFloat()
                    Expect.isTrue true "All IRandom methods should work"

                testCase "IRandom interface can be implemented by RandThreadSafe"
                <| fun () ->
                    let rng: Random.IRandom = Random.RandThreadSafe(42) :> Random.IRandom
                    let _ = rng.NextInt()
                    let _ = rng.NextInt(100)
                    let _ = rng.NextFloat()
                    Expect.isTrue true "All IRandom methods should work" ] ]
