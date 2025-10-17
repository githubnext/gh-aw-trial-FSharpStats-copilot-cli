module RunningStatsTests

open Expecto
open FSharp.Stats
open FSharp.Stats.SummaryStats
open FSharp.Stats.RunningStats

[<Tests>]
let summaryStatsTests =
    testList
        "SummaryStats"
        [ testCase "createSummaryStats creates expected structure"
          <| fun () ->
              let stats = createSummaryStats 10.0 5.0 25.0 1.0 9.0
              Expect.equal stats.N 10.0 "N should be 10"
              Expect.equal stats.Mean 5.0 "Mean should be 5"
              Expect.equal stats.SumOfSquares 25.0 "SumOfSquares should be 25"
              Expect.equal stats.Min 1.0 "Min should be 1"
              Expect.equal stats.Max 9.0 "Max should be 9"

          testCase "mean returns correct value"
          <| fun () ->
              let stats = createSummaryStats 100.0 42.5 100.0 1.0 100.0
              let result = SummaryStats.mean stats
              Expect.equal result 42.5 "Mean should be 42.5"

          testCase "varPopulation calculates population variance"
          <| fun () ->
              let stats = createSummaryStats 10.0 5.0 90.0 1.0 9.0
              let result = SummaryStats.varPopulation stats
              Expect.equal result 9.0 "Population variance should be 9 (90/10)"

          testCase "var calculates sample variance"
          <| fun () ->
              let stats = createSummaryStats 10.0 5.0 90.0 1.0 9.0
              let result = SummaryStats.var stats
              Expect.equal result 10.0 "Sample variance should be 10 (90/9)"

          testCase "stDevPopulation calculates population standard deviation"
          <| fun () ->
              let stats = createSummaryStats 10.0 5.0 90.0 1.0 9.0
              let result = SummaryStats.stDevPopulation stats
              Expect.equal result 3.0 "Population stdev should be 3 (sqrt(9))"

          testCase "stDev calculates sample standard deviation"
          <| fun () ->
              let stats = createSummaryStats 10.0 5.0 90.0 1.0 9.0
              let result = SummaryStats.stDev stats
              let expected = sqrt 10.0
              Expect.floatClose Accuracy.high result expected "Sample stdev should be sqrt(10)"

          testCase "SummaryStats with single observation"
          <| fun () ->
              let stats = createSummaryStats 1.0 5.0 0.0 5.0 5.0
              Expect.equal (SummaryStats.mean stats) 5.0 "Mean of single value"
              Expect.equal (SummaryStats.varPopulation stats) 0.0 "Population variance of single value is 0"

          testCase "SummaryStats with zero sum of squares"
          <| fun () ->
              let stats = createSummaryStats 5.0 10.0 0.0 10.0 10.0
              Expect.equal (SummaryStats.varPopulation stats) 0.0 "Population variance should be 0"
              Expect.equal (SummaryStats.stDevPopulation stats) 0.0 "Population stdev should be 0" ]

[<Tests>]
let runningStatsTests =
    testList
        "RunningStats"
        [ testCase "createRunningStats creates expected structure"
          <| fun () ->
              let stats = createRunningStats 10 5.0 25.0 0.5 2.0
              Expect.equal stats.N 10 "N should be 10"
              Expect.equal stats.M1 5.0 "M1 should be 5"
              Expect.equal stats.M2 25.0 "M2 should be 25"
              Expect.equal stats.M3 0.5 "M3 should be 0.5"
              Expect.equal stats.M4 2.0 "M4 should be 2"

          testCase "mean returns M1"
          <| fun () ->
              let stats = createRunningStats 100 42.5 100.0 10.0 50.0
              let result = RunningStats.mean stats
              Expect.equal result 42.5 "Mean should equal M1"

          testCase "varPopulation calculates population variance"
          <| fun () ->
              let stats = createRunningStats 10 5.0 90.0 0.0 0.0
              let result = RunningStats.varPopulation stats
              Expect.equal result 9.0 "Population variance should be 9 (90/10)"

          testCase "var calculates sample variance"
          <| fun () ->
              let stats = createRunningStats 10 5.0 90.0 0.0 0.0
              let result = RunningStats.var stats
              Expect.equal result 10.0 "Sample variance should be 10 (90/9)"

          testCase "stDevPopulation calculates population standard deviation"
          <| fun () ->
              let stats = createRunningStats 10 5.0 90.0 0.0 0.0
              let result = RunningStats.stDevPopulation stats
              Expect.equal result 3.0 "Population stdev should be 3 (sqrt(9))"

          testCase "stDev calculates sample standard deviation"
          <| fun () ->
              let stats = createRunningStats 10 5.0 90.0 0.0 0.0
              let result = RunningStats.stDev stats
              let expected = sqrt 10.0
              Expect.floatClose Accuracy.high result expected "Sample stdev should be sqrt(10)"

          testCase "RunningStats with single observation"
          <| fun () ->
              let stats = createRunningStats 1 5.0 0.0 0.0 0.0
              Expect.equal (RunningStats.mean stats) 5.0 "Mean of single value"
              Expect.equal (RunningStats.varPopulation stats) 0.0 "Population variance of single value is 0"

          testCase "RunningStats with zero M2"
          <| fun () ->
              let stats = createRunningStats 5 10.0 0.0 0.0 0.0
              Expect.equal (RunningStats.varPopulation stats) 0.0 "Population variance should be 0"
              Expect.equal (RunningStats.stDevPopulation stats) 0.0 "Population stdev should be 0"

          testCase "RunningStats with larger sample size"
          <| fun () ->
              // Simulate stats for [1,2,3,4,5] where mean=3, var=2.5
              // M2 = sum((x-mean)^2) = 4+1+0+1+4 = 10
              let stats = createRunningStats 5 3.0 10.0 0.0 0.0
              Expect.equal (RunningStats.mean stats) 3.0 "Mean should be 3"
              Expect.equal (RunningStats.varPopulation stats) 2.0 "Population variance should be 2"
              Expect.equal (RunningStats.var stats) 2.5 "Sample variance should be 2.5"
              let expectedStDev = sqrt 2.5
              Expect.floatClose Accuracy.high (RunningStats.stDev stats) expectedStDev "Sample stdev"

          testCase "RunningStats variance edge case with n=2"
          <| fun () ->
              // With n=2, sample variance divides by 1
              let stats = createRunningStats 2 5.0 8.0 0.0 0.0
              Expect.equal (RunningStats.var stats) 8.0 "Sample variance with n=2 should be M2/(2-1)=M2" ]
