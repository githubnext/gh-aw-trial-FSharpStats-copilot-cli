module ListTests

open Expecto
open System
open FSharp.Stats
open TestExtensions

let testListEvenCounts = [10000.;-0.1;14.;-10.]
let testListOddCounts = [10000.;-0.1;14.;-10.;5.]
let testListNan = [10000.;-0.1;14.;-10.;5.;Double.NaN]
let testListInfinity = [10000.;-0.1;14.;-10.;Double.PositiveInfinity]
let testListNegInfinity = [10000.;-0.1;14.;-10.;5.;Double.NegativeInfinity]

let testListEvenCountsInt = [10000;-50;14;-9]
let testListOddCountsInt = [10000;-50;14;-10;5]
let testListEmptyInt : int list = []
let testListEmptyFloat : float list = []

[<Tests>]
let rangeTests =
    testList "List.range" [
        testCase "range with positive values" <| fun () ->
            let testList = [1.0; 5.0; 3.0; 9.0; 2.0]
            let range = List.range testList
            Expect.equal (Interval.getStart range) 1.0 "Range start should be 1.0"
            Expect.equal (Interval.getEnd range) 9.0 "Range end should be 9.0"
        
        testCase "range with negative values" <| fun () ->
            let testList = [-5.0; -1.0; -10.0; -2.0]
            let range = List.range testList
            Expect.equal (Interval.getStart range) -10.0 "Range start should be -10.0"
            Expect.equal (Interval.getEnd range) -1.0 "Range end should be -1.0"
        
        testCase "range with mixed values" <| fun () ->
            let testList = [-5.0; 10.0; 0.0; 3.0]
            let range = List.range testList
            Expect.equal (Interval.getStart range) -5.0 "Range start should be -5.0"
            Expect.equal (Interval.getEnd range) 10.0 "Range end should be 10.0"
        
        testCase "range with single value" <| fun () ->
            let testList = [42.0]
            let range = List.range testList
            Expect.equal (Interval.getStart range) 42.0 "Range start should be 42.0"
            Expect.equal (Interval.getEnd range) 42.0 "Range end should be 42.0"
        
        testCase "range with empty list" <| fun () ->
            let testList : float list = []
            let range = List.range testList
            Expect.equal range Interval.Empty "Range should be empty for empty list"
    ]

[<Tests>]
let medianTests =
    testList "List.median" [
        testCase "medianEvenCounts" <| fun () ->
            let median = List.median testListEvenCounts
            Expect.floatClose Accuracy.high median 6.95 "Median should be 6.95"
        testCase "medianOddCounts" <| fun () ->
            let median = List.median testListOddCounts
            Expect.floatClose Accuracy.high median 5. "Median should be 5.0"
        testCase "medianNan" <| fun () ->
            let median = List.median testListNan
            Expect.isTrue (Double.IsNaN median) "Median should be nan"
        testCase "medianInf" <| fun () ->
            let median = List.median testListInfinity
            Expect.floatClose Accuracy.high median 14. "Median should be 14.0"
        testCase "medianNegInf" <| fun () ->
            let median = List.median testListNegInfinity
            Expect.floatClose Accuracy.high median 2.45 "Median should be 2.45"
        
        testCase "testListEvenCountsInt" <| fun () ->
            let median = List.median testListEvenCountsInt
            Expect.equal median 2 "Median should be 2"
        testCase "testListOddCountsInt" <| fun () ->
            let median = List.median testListOddCountsInt
            Expect.equal median 5 "Median should be 5"
        
        testCase "median single value" <| fun () ->
            let testList = [42.0]
            let median = List.median testList
            Expect.floatClose Accuracy.high median 42.0 "Median should be 42.0"
        
        testCase "median sorted list" <| fun () ->
            let testList = [1.0; 2.0; 3.0; 4.0; 5.0]
            let median = List.median testList
            Expect.floatClose Accuracy.high median 3.0 "Median should be 3.0"
        
        testCase "median reverse sorted list" <| fun () ->
            let testList = [5.0; 4.0; 3.0; 2.0; 1.0]
            let median = List.median testList
            Expect.floatClose Accuracy.high median 3.0 "Median should be 3.0"
        
        testCase "median empty list should fail" <| fun () ->
            let median() = List.median testListEmptyFloat
            Expect.throws (fun () -> median () |> ignore) "Median for empty list should throw"
    ]

[<Tests>]
let meanTests =
    testList "List.mean" [
        testCase "mean" <| fun () ->
            let mean = List.mean testListEvenCounts
            Expect.floatClose Accuracy.high mean 2500.975 "Mean should be 2500.975"
        testCase "meanNan" <| fun () ->
            let mean = List.mean testListNan
            Expect.isTrue (Double.IsNaN mean) "Mean should be nan"
        testCase "meanInf" <| fun () ->
            let mean = List.mean testListInfinity
            Expect.isTrue (Double.IsInfinity mean) "Mean should be inf"
        testCase "meanNegInf" <| fun () ->
            let mean = List.mean testListNegInfinity
            Expect.isTrue (Double.IsNegativeInfinity mean) "Mean should be nan"
        
        testCase "mean of positive floats" <| fun () ->
            let testList = [1.0; 2.0; 3.0; 4.0; 5.0]
            let mean = List.mean testList
            Expect.floatClose Accuracy.high mean 3.0 "Mean should be 3.0"
        
        testCase "mean of integers" <| fun () ->
            let testList = [10; 20; 30; 40]
            let mean = List.mean testList
            Expect.equal mean 25 "Mean should be 25"
        
        testCase "mean with negative values" <| fun () ->
            let testList = [-10.0; -5.0; 5.0; 10.0]
            let mean = List.mean testList
            Expect.floatClose Accuracy.high mean 0.0 "Mean should be 0.0"
        
        testCase "mean of single value" <| fun () ->
            let testList = [42.0]
            let mean = List.mean testList
            Expect.floatClose Accuracy.high mean 42.0 "Mean should be 42.0"
    ]

[<Tests>]
let covarianceTests =
    testList "List.covariance" [
        testCase "covPopulation basic" <| fun () ->
            let list1 = [1.0; 2.0; 3.0; 4.0; 5.0]
            let list2 = [2.0; 4.0; 6.0; 8.0; 10.0]
            let cov = List.covPopulation list1 list2
            Expect.floatClose Accuracy.high cov 4.0 "Population covariance should be 4.0"
        
        testCase "cov (sample) basic" <| fun () ->
            let list1 = [1.0; 2.0; 3.0; 4.0; 5.0]
            let list2 = [2.0; 4.0; 6.0; 8.0; 10.0]
            let cov = List.cov list1 list2
            Expect.floatClose Accuracy.high cov 5.0 "Sample covariance should be 5.0"
        
        testCase "covPopulationOfPairs" <| fun () ->
            let pairs = [(5., 2.); (12., 8.); (18., 18.); (-23., -20.); (45., 28.)]
            let cov = List.covPopulationOfPairs pairs
            Expect.floatClose Accuracy.medium cov 347.92 "Population covariance of pairs should be 347.92"
        
        testCase "covOfPairs" <| fun () ->
            let pairs = [(5., 2.); (12., 8.); (18., 18.); (-23., -20.); (45., 28.)]
            let cov = List.covOfPairs pairs
            Expect.floatClose Accuracy.medium cov 434.90 "Sample covariance of pairs should be 434.90"
        
        testCase "covPopulationBy" <| fun () ->
            let data = [ {| x = 5.; y = 2. |}
                         {| x = 12.; y = 8. |}
                         {| x = 18.; y = 18. |}
                         {| x = -23.; y = -20. |} 
                         {| x = 45.; y = 28. |} ]
            let cov : float = List.covPopulationBy (fun (x: {| x: float; y: float |}) -> x.x, x.y) data
            Expect.floatClose Accuracy.medium cov 347.92 "Population covariance by function should be 347.92"
        
        testCase "covBy" <| fun () ->
            let data = [ {| x = 5.; y = 2. |}
                         {| x = 12.; y = 8. |}
                         {| x = 18.; y = 18. |}
                         {| x = -23.; y = -20. |} 
                         {| x = 45.; y = 28. |} ]
            let cov : float = List.covBy (fun (x: {| x: float; y: float |}) -> x.x, x.y) data
            Expect.floatClose Accuracy.medium cov 434.90 "Sample covariance by function should be 434.90"
        
        testCase "covPopulation with identical lists" <| fun () ->
            let list1 = [1.0; 2.0; 3.0; 4.0; 5.0]
            let cov = List.covPopulation list1 list1
            Expect.floatClose Accuracy.high cov 2.0 "Population covariance of identical lists should equal variance"
        
        testCase "cov with negative correlation" <| fun () ->
            let list1 = [1.0; 2.0; 3.0; 4.0; 5.0]
            let list2 = [5.0; 4.0; 3.0; 2.0; 1.0]
            let cov = List.cov list1 list2
            Expect.isTrue (cov < 0.0) "Sample covariance should be negative for negatively correlated lists"
    ]

[<Tests>]
let linspaceTests =
    testList "List.linspace" [
        testCase "linspace basic range" <| fun () ->
            let expected = [-3.5; 2.1; 7.7; 13.3; 18.9; 24.5; 30.1]
            let actual = List.linspace(start= -3.5, stop= 30.1, num=7)
            TestExtensions.sequenceEqual (Accuracy.high) actual expected "linspace results in wrong list"
        
        testCase "linspace with many points" <| fun () ->
            let expected = [-3.5; -3.1; -2.7; -2.3; -1.9; -1.5; -1.1; -0.7; -0.3; 0.1; 0.5; 0.9; 1.3; 1.7; 2.1; 2.5; 2.9]
            let actual = List.linspace(start= -3.5, stop= 2.9, num=17)
            TestExtensions.sequenceEqual (Accuracy.high) actual expected "linspace results in wrong list"
        
        testCase "linspace without endpoint" <| fun () ->
            let expected = [-3.5; 2.1; 7.7; 13.3; 18.9; 24.5]
            let actual = List.linspace(start= -3.5, stop= 30.1, num=6, IncludeEndpoint=false)
            TestExtensions.sequenceEqual (Accuracy.high) actual expected "linspace without endpoint results in wrong list"
        
        testCase "linspace single point" <| fun () ->
            let actual = List.linspace(start= 5.0, stop= 10.0, num=1)
            Expect.equal (List.length actual) 1 "linspace with num=1 should return single element"
        
        testCase "linspace two points" <| fun () ->
            let expected = [0.0; 10.0]
            let actual = List.linspace(start= 0.0, stop= 10.0, num=2)
            TestExtensions.sequenceEqual (Accuracy.high) actual expected "linspace with num=2 should return start and end"
    ]

[<Tests>]
let geomspaceTests =
    testList "List.geomspace" [
        testCase "geomspace basic range" <| fun () ->
            let actual = List.geomspace(start= 1.0, stop= 1000.0, num=4)
            let expected = [1.0; 10.0; 100.0; 1000.0]
            TestExtensions.sequenceEqual (Accuracy.medium) actual expected "geomspace results in wrong list"
        
        testCase "geomspace without endpoint" <| fun () ->
            let actual : float list = List.geomspace(start= 1.0, stop= 1000.0, num=3, IncludeEndpoint=false)
            Expect.equal (List.length actual) 3 "geomspace should return 3 elements"
            Expect.floatClose Accuracy.medium (actual |> List.head) 1.0 "First element should be 1.0"
        
        testCase "geomspace single point" <| fun () ->
            let actual = List.geomspace(start= 1.0, stop= 100.0, num=1)
            Expect.equal (List.length actual) 1 "geomspace with num=1 should return single element"
        
        testCase "geomspace two points" <| fun () ->
            let actual : float list = List.geomspace(start= 1.0, stop= 100.0, num=2)
            Expect.equal (List.length actual) 2 "geomspace with num=2 should return two elements"
            Expect.floatClose Accuracy.high (actual |> List.head) 1.0 "First element should be start value"
            Expect.floatClose Accuracy.high (actual |> List.last) 100.0 "Last element should be end value"
    ]
