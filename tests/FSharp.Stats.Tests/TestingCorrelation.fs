module TestingCorrelation

open Expecto
open FSharp.Stats
open FSharp.Stats.Testing

[<Tests>]
let correlationTests =
    testList
        "Testing.Correlation"
        [

          testList
              "testPearson"
              [ test "Perfect positive correlation" {
                    let seq1 =
                        [ 1.0
                          2.0
                          3.0
                          4.0
                          5.0 ]
                    let seq2 =
                        [ 2.0
                          4.0
                          6.0
                          8.0
                          10.0 ]
                    let result = Correlation.testPearson seq1 seq2

                    // Perfect correlation means r=1, so t will be very large (approaching infinity)
                    Expect.isTrue (result.Statistic > 10.0) "t-statistic should be very high for perfect correlation"
                    Expect.equal result.DegreesOfFreedom 3.0 "DOF should be n-2"
                    // For one-tailed test with positive correlation, check PValueRight
                    Expect.isTrue (result.PValueRight < 0.001) "P-value should be very small for perfect correlation"
                }

                test "Perfect negative correlation" {
                    let seq1 =
                        [ 1.0
                          2.0
                          3.0
                          4.0
                          5.0 ]
                    let seq2 =
                        [ 10.0
                          8.0
                          6.0
                          4.0
                          2.0 ]
                    let result = Correlation.testPearson seq1 seq2

                    Expect.isTrue (result.Statistic < 0.0) "t-statistic should be negative for negative correlation"
                    Expect.equal result.DegreesOfFreedom 3.0 "DOF should be n-2"
                }

                test "No correlation" {
                    let seq1 =
                        [ 1.0
                          2.0
                          3.0
                          4.0
                          5.0 ]
                    let seq2 =
                        [ 5.0
                          2.0
                          4.0
                          1.0
                          3.0 ] // Changed to truly have low correlation
                    let result = Correlation.testPearson seq1 seq2

                    // With truly uncorrelated data, t-statistic should be small
                    Expect.isTrue (abs result.Statistic < 2.0) "t-statistic should be near 0 for no correlation"
                }

                test "Weak positive correlation" {
                    let seq1 =
                        [ 1.0
                          2.0
                          3.0
                          4.0
                          5.0
                          6.0
                          7.0
                          8.0 ]
                    let seq2 =
                        [ 2.1
                          3.9
                          6.2
                          7.8
                          10.1
                          11.9
                          14.2
                          15.8 ]
                    let result = Correlation.testPearson seq1 seq2

                    Expect.isTrue (result.Statistic > 0.0) "t-statistic should be positive"
                    Expect.equal result.DegreesOfFreedom 6.0 "DOF should be n-2"
                }

                test "Small sample size" {
                    let seq1 =
                        [ 1.0
                          2.0
                          3.0 ]
                    let seq2 =
                        [ 2.0
                          4.0
                          6.0 ]
                    let result = Correlation.testPearson seq1 seq2

                    Expect.equal result.DegreesOfFreedom 1.0 "DOF should be 1 for n=3"
                } ]

          testList
              "testPearsonByPermutation"
              [ test "Perfect correlation should give low p-value" {
                    let seq1 =
                        [| 1.0
                           2.0
                           3.0
                           4.0
                           5.0 |]
                    let seq2 =
                        [| 2.0
                           4.0
                           6.0
                           8.0
                           10.0 |]
                    let pValue = Correlation.testPearsonByPermutation seq1 seq2 1000

                    // For perfect correlation, p-value should be small (but permutation tests may not be 0)
                    Expect.isTrue (pValue <= 0.05) "p-value should be small for perfect correlation"
                }

                test "No correlation should give high p-value" {
                    let seq1 =
                        [| 1.0
                           2.0
                           3.0
                           4.0
                           5.0
                           6.0
                           7.0
                           8.0
                           9.0
                           10.0 |]
                    let seq2 =
                        [| 5.0
                           3.0
                           7.0
                           2.0
                           9.0
                           1.0
                           8.0
                           4.0
                           10.0
                           6.0 |]
                    let pValue = Correlation.testPearsonByPermutation seq1 seq2 1000

                    Expect.isTrue (pValue > 0.1) "p-value should be high for random data"
                }

                test "Negative correlation detected" {
                    let seq1 =
                        [| 1.0
                           2.0
                           3.0
                           4.0
                           5.0 |]
                    let seq2 =
                        [| 10.0
                           8.0
                           6.0
                           4.0
                           2.0 |]
                    let pValue = Correlation.testPearsonByPermutation seq1 seq2 1000

                    Expect.isTrue (pValue <= 0.05) "p-value should be small for strong negative correlation"
                }

                test "With few permutations" {
                    let seq1 =
                        [| 1.0
                           2.0
                           3.0
                           4.0 |]
                    let seq2 =
                        [| 2.0
                           4.0
                           6.0
                           8.0 |]
                    let pValue = Correlation.testPearsonByPermutation seq1 seq2 100

                    Expect.isTrue (pValue >= 0.0 && pValue <= 1.0) "p-value should be between 0 and 1"
                }

                test "With many permutations for precision" {
                    let seq1 =
                        [| 1.0
                           2.0
                           3.0
                           4.0
                           5.0 |]
                    let seq2 =
                        [| 2.5
                           4.1
                           5.9
                           8.2
                           10.1 |]
                    let pValue = Correlation.testPearsonByPermutation seq1 seq2 5000

                    Expect.isTrue (pValue >= 0.0 && pValue <= 1.0) "p-value should be between 0 and 1"
                } ]

          testList
              "testSpearmanByPermutation"
              [ test "Monotonic increasing relationship" {
                    let seq1 =
                        [| 1.0
                           2.0
                           3.0
                           4.0
                           5.0 |]
                    let seq2 =
                        [| 1.0
                           4.0
                           9.0
                           16.0
                           25.0 |] // x^2 - monotonic but not linear
                    let pValue = Correlation.testSpearmanByPermutation seq1 seq2 1000

                    Expect.isTrue (pValue <= 0.05) "p-value should be small for monotonic relationship"
                }

                test "Perfect rank correlation" {
                    let seq1 =
                        [| 1.0
                           2.0
                           3.0
                           4.0
                           5.0 |]
                    let seq2 =
                        [| 10.0
                           20.0
                           30.0
                           40.0
                           50.0 |]
                    let pValue = Correlation.testSpearmanByPermutation seq1 seq2 1000

                    Expect.isTrue (pValue <= 0.05) "p-value should be small for perfect rank correlation"
                }

                test "No monotonic relationship" {
                    let seq1 =
                        [| 1.0
                           2.0
                           3.0
                           4.0
                           5.0
                           6.0
                           7.0
                           8.0 |]
                    let seq2 =
                        [| 4.0
                           2.0
                           6.0
                           1.0
                           7.0
                           3.0
                           8.0
                           5.0 |]
                    let pValue = Correlation.testSpearmanByPermutation seq1 seq2 1000

                    Expect.isTrue (pValue > 0.1) "p-value should be high for no relationship"
                }

                test "Inverse monotonic relationship" {
                    let seq1 =
                        [| 1.0
                           2.0
                           3.0
                           4.0
                           5.0 |]
                    let seq2 =
                        [| 25.0
                           16.0
                           9.0
                           4.0
                           1.0 |]
                    let pValue = Correlation.testSpearmanByPermutation seq1 seq2 1000

                    Expect.isTrue (pValue <= 0.05) "p-value should be small for inverse monotonic"
                }

                test "With outliers - Spearman more robust" {
                    let seq1 =
                        [| 1.0
                           2.0
                           3.0
                           4.0
                           5.0 |]
                    let seq2 =
                        [| 2.0
                           4.0
                           6.0
                           8.0
                           100.0 |] // outlier at end
                    let pValue = Correlation.testSpearmanByPermutation seq1 seq2 1000

                    // Spearman should still detect monotonic trend despite outlier
                    Expect.isTrue (pValue <= 0.1) "Spearman should be robust to outliers"
                } ]

          testList
              "testKendallByPermutation"
              [ test "Perfect concordance" {
                    let seq1 =
                        [| 1.0
                           2.0
                           3.0
                           4.0
                           5.0 |]
                    let seq2 =
                        [| 1.0
                           2.0
                           3.0
                           4.0
                           5.0 |]
                    let pValue = Correlation.testKendallByPermutation seq1 seq2 1000

                    Expect.isTrue (pValue <= 0.05) "p-value should be small for perfect concordance"
                }

                test "Perfect discordance" {
                    let seq1 =
                        [| 1.0
                           2.0
                           3.0
                           4.0
                           5.0 |]
                    let seq2 =
                        [| 5.0
                           4.0
                           3.0
                           2.0
                           1.0 |]
                    let pValue = Correlation.testKendallByPermutation seq1 seq2 1000

                    Expect.isTrue (pValue <= 0.05) "p-value should be small for perfect discordance"
                }

                test "Random ordering" {
                    let seq1 =
                        [| 1.0
                           2.0
                           3.0
                           4.0
                           5.0
                           6.0
                           7.0 |]
                    let seq2 =
                        [| 4.0
                           1.0
                           6.0
                           3.0
                           7.0
                           2.0
                           5.0 |]
                    let pValue = Correlation.testKendallByPermutation seq1 seq2 1000

                    Expect.isTrue (pValue > 0.1) "p-value should be high for random data"
                }

                test "Moderate positive concordance" {
                    let seq1 =
                        [| 1.0
                           2.0
                           3.0
                           4.0
                           5.0
                           6.0 |]
                    let seq2 =
                        [| 1.0
                           3.0
                           2.0
                           4.0
                           6.0
                           5.0 |] // mostly ordered
                    let pValue = Correlation.testKendallByPermutation seq1 seq2 1000

                    Expect.isTrue (pValue <= 0.2) "p-value should show some significance"
                }

                test "Small sample" {
                    let seq1 =
                        [| 1.0
                           2.0
                           3.0 |]
                    let seq2 =
                        [| 3.0
                           2.0
                           1.0 |]
                    let pValue = Correlation.testKendallByPermutation seq1 seq2 500

                    Expect.isTrue (pValue >= 0.0 && pValue <= 1.0) "p-value should be valid"
                } ]

          testList
              "Edge cases and validation"
              [ test "testPearson with same values" {
                    let seq1 =
                        [ 1.0
                          1.0
                          1.0
                          1.0 ]
                    let seq2 =
                        [ 2.0
                          2.0
                          2.0
                          2.0 ]

                    // This should handle the case where variance is 0
                    // The correlation will be NaN or undefined
                    let result = Correlation.testPearson seq1 seq2
                    Expect.isTrue
                        (System.Double.IsNaN(result.Statistic)
                         || System.Double.IsInfinity(result.Statistic))
                        "Should handle zero variance case"
                }

                test "Permutation tests return valid probabilities" {
                    let seq1 =
                        [| 1.0
                           2.0
                           3.0
                           4.0
                           5.0 |]
                    let seq2 =
                        [| 1.5
                           3.2
                           4.8
                           7.1
                           9.5 |]

                    let pPearson = Correlation.testPearsonByPermutation seq1 seq2 1000
                    let pSpearman = Correlation.testSpearmanByPermutation seq1 seq2 1000
                    let pKendall = Correlation.testKendallByPermutation seq1 seq2 1000

                    Expect.isTrue (pPearson >= 0.0 && pPearson <= 1.0) "Pearson p-value in valid range"
                    Expect.isTrue (pSpearman >= 0.0 && pSpearman <= 1.0) "Spearman p-value in valid range"
                    Expect.isTrue (pKendall >= 0.0 && pKendall <= 1.0) "Kendall p-value in valid range"
                }

                test "Two-tailed test catches both positive and negative correlations" {
                    let seq1 =
                        [| 1.0
                           2.0
                           3.0
                           4.0
                           5.0 |]
                    let seq2Pos =
                        [| 2.0
                           4.0
                           6.0
                           8.0
                           10.0 |]
                    let seq2Neg =
                        [| 10.0
                           8.0
                           6.0
                           4.0
                           2.0 |]

                    let pPos = Correlation.testPearsonByPermutation seq1 seq2Pos 1000
                    let pNeg = Correlation.testPearsonByPermutation seq1 seq2Neg 1000

                    // Both should show significant correlation (two-tailed)
                    Expect.isTrue (pPos < 0.05) "Positive correlation should be significant"
                    Expect.isTrue (pNeg < 0.05) "Negative correlation should be significant"
                } ] ]
