module MapTests

open Expecto
open FSharp.Stats

[<Tests>]
let mapTests =
    testList
        "Map"
        [

          testList
              "mergeBy"
              [ testCase "merges two maps with custom function"
                <| fun () ->
                    let mapA =
                        Map.ofList
                            [ ("a", 1)
                              ("b", 2)
                              ("c", 3) ]
                    let mapB =
                        Map.ofList
                            [ ("b", 10)
                              ("c", 20)
                              ("d", 40) ]
                    let result = Map.mergeBy (fun a b -> a + b) mapA mapB

                    Expect.equal result.["a"] 1 "Key only in mapA should have value from mapA"
                    Expect.equal result.["b"] 12 "Key in both maps should use custom function (2+10=12)"
                    Expect.equal result.["c"] 23 "Key in both maps should use custom function (3+20=23)"
                    Expect.equal result.["d"] 40 "Key only in mapB should have value from mapB"

                testCase "handles non-overlapping maps"
                <| fun () ->
                    let mapA =
                        Map.ofList
                            [ ("a", 1)
                              ("b", 2) ]
                    let mapB =
                        Map.ofList
                            [ ("c", 3)
                              ("d", 4) ]
                    let result = Map.mergeBy (fun a b -> a * b) mapA mapB

                    Expect.equal result.Count 4 "Should have all keys from both maps"
                    Expect.equal result.["a"] 1 "Key only in mapA"
                    Expect.equal result.["b"] 2 "Key only in mapA"
                    Expect.equal result.["c"] 3 "Key only in mapB"
                    Expect.equal result.["d"] 4 "Key only in mapB"

                testCase "handles empty mapA"
                <| fun () ->
                    let mapA = Map.empty
                    let mapB =
                        Map.ofList
                            [ ("a", 1)
                              ("b", 2) ]
                    let result = Map.mergeBy (fun a b -> a + b) mapA mapB

                    Expect.equal result mapB "Empty mapA should result in mapB"

                testCase "handles empty mapB"
                <| fun () ->
                    let mapA =
                        Map.ofList
                            [ ("a", 1)
                              ("b", 2) ]
                    let mapB = Map.empty
                    let result = Map.mergeBy (fun a b -> a + b) mapA mapB

                    Expect.equal result mapA "Empty mapB should result in mapA"

                testCase "handles both empty maps"
                <| fun () ->
                    let mapA: Map<string, int> = Map.empty
                    let mapB: Map<string, int> = Map.empty
                    let result = Map.mergeBy (fun a b -> a + b) mapA mapB

                    Expect.equal result Map.empty "Both empty maps should result in empty map"

                testCase "function receives correct arguments"
                <| fun () ->
                    let mapA = Map.ofList [ ("x", 10) ]
                    let mapB = Map.ofList [ ("x", 5) ]
                    let result = Map.mergeBy (fun a b -> a - b) mapA mapB

                    Expect.equal result.["x"] 5 "First arg from mapA, second from mapB (10-5=5)"

                testCase "works with different types"
                <| fun () ->
                    let mapA =
                        Map.ofList
                            [ (1, "hello")
                              (2, "world") ]
                    let mapB =
                        Map.ofList
                            [ (2, " there")
                              (3, "!") ]
                    let result = Map.mergeBy (fun a b -> a + b) mapA mapB

                    Expect.equal result.[1] "hello" "Key 1 from mapA"
                    Expect.equal result.[2] "world there" "Key 2 concatenated"
                    Expect.equal result.[3] "!" "Key 3 from mapB"

                testCase "handles float values"
                <| fun () ->
                    let mapA =
                        Map.ofList
                            [ ("a", 1.5)
                              ("b", 2.5) ]
                    let mapB =
                        Map.ofList
                            [ ("b", 3.5)
                              ("c", 4.5) ]
                    let result = Map.mergeBy (fun a b -> a * b) mapA mapB

                    Expect.equal result.["a"] 1.5 "Single key from mapA"
                    Expect.floatClose Accuracy.high result.["b"] 8.75 "2.5 * 3.5 = 8.75"
                    Expect.equal result.["c"] 4.5 "Single key from mapB" ]

          testList
              "merge"
              [ testCase "mapB values supersede mapA values"
                <| fun () ->
                    let mapA =
                        Map.ofList
                            [ ("a", 1)
                              ("b", 2)
                              ("c", 3) ]
                    let mapB =
                        Map.ofList
                            [ ("b", 20)
                              ("c", 30)
                              ("d", 40) ]
                    let result = Map.merge mapA mapB

                    Expect.equal result.["a"] 1 "Key only in mapA"
                    Expect.equal result.["b"] 20 "mapB value supersedes mapA (20 not 2)"
                    Expect.equal result.["c"] 30 "mapB value supersedes mapA (30 not 3)"
                    Expect.equal result.["d"] 40 "Key only in mapB"

                testCase "is not commutative"
                <| fun () ->
                    let mapA = Map.ofList [ ("x", 1) ]
                    let mapB = Map.ofList [ ("x", 2) ]
                    let resultAB = Map.merge mapA mapB
                    let resultBA = Map.merge mapB mapA

                    Expect.equal resultAB.["x"] 2 "merge(A,B) uses B's value"
                    Expect.equal resultBA.["x"] 1 "merge(B,A) uses A's value"
                    Expect.notEqual resultAB resultBA "merge is not commutative"

                testCase "handles empty maps"
                <| fun () ->
                    let mapA = Map.ofList [ ("a", 1) ]
                    let mapB = Map.empty
                    let result1 = Map.merge mapA mapB
                    let result2 = Map.merge mapB mapA

                    Expect.equal result1 mapA "merge with empty mapB returns mapA"
                    Expect.equal result2 mapA "merge empty with mapA returns mapA"

                testCase "all keys from mapB are present"
                <| fun () ->
                    let mapA = Map.ofList [ ("a", 1) ]
                    let mapB =
                        Map.ofList
                            [ ("b", 2)
                              ("c", 3) ]
                    let result = Map.merge mapA mapB

                    Expect.isTrue (result.ContainsKey "b") "Should contain mapB keys"
                    Expect.isTrue (result.ContainsKey "c") "Should contain mapB keys" ]

          testList
              "mergeSubtract"
              [ testCase "subtracts mapB values from mapA values"
                <| fun () ->
                    let mapA =
                        Map.ofList
                            [ ("a", 10)
                              ("b", 20)
                              ("c", 30) ]
                    let mapB =
                        Map.ofList
                            [ ("b", 5)
                              ("c", 10)
                              ("d", 15) ]
                    let result = Map.mergeSubtract mapA mapB

                    Expect.equal result.["a"] 10 "Key only in mapA"
                    Expect.equal result.["b"] 15 "20 - 5 = 15"
                    Expect.equal result.["c"] 20 "30 - 10 = 20"
                    Expect.equal result.["d"] 15 "Key only in mapB"

                testCase "handles negative results"
                <| fun () ->
                    let mapA = Map.ofList [ ("x", 5) ]
                    let mapB = Map.ofList [ ("x", 10) ]
                    let result = Map.mergeSubtract mapA mapB

                    Expect.equal result.["x"] -5 "5 - 10 = -5"

                testCase "is not commutative"
                <| fun () ->
                    let mapA = Map.ofList [ ("x", 10) ]
                    let mapB = Map.ofList [ ("x", 3) ]
                    let resultAB = Map.mergeSubtract mapA mapB
                    let resultBA = Map.mergeSubtract mapB mapA

                    Expect.equal resultAB.["x"] 7 "10 - 3 = 7"
                    Expect.equal resultBA.["x"] -7 "3 - 10 = -7"
                    Expect.notEqual resultAB resultBA "mergeSubtract is not commutative"

                testCase "works with floats"
                <| fun () ->
                    let mapA =
                        Map.ofList
                            [ ("a", 10.5)
                              ("b", 20.5) ]
                    let mapB =
                        Map.ofList
                            [ ("a", 0.5)
                              ("c", 5.0) ]
                    let result = Map.mergeSubtract mapA mapB

                    Expect.floatClose Accuracy.high result.["a"] 10.0 "10.5 - 0.5 = 10.0"
                    Expect.equal result.["b"] 20.5 "Key only in mapA"
                    Expect.equal result.["c"] 5.0 "Key only in mapB"

                testCase "handles zero results"
                <| fun () ->
                    let mapA = Map.ofList [ ("x", 5) ]
                    let mapB = Map.ofList [ ("x", 5) ]
                    let result = Map.mergeSubtract mapA mapB

                    Expect.equal result.["x"] 0 "5 - 5 = 0" ]

          testList
              "mergeAdd"
              [ testCase "adds mapB values to mapA values"
                <| fun () ->
                    let mapA =
                        Map.ofList
                            [ ("a", 10)
                              ("b", 20)
                              ("c", 30) ]
                    let mapB =
                        Map.ofList
                            [ ("b", 5)
                              ("c", 10)
                              ("d", 15) ]
                    let result = Map.mergeAdd mapA mapB

                    Expect.equal result.["a"] 10 "Key only in mapA"
                    Expect.equal result.["b"] 25 "20 + 5 = 25"
                    Expect.equal result.["c"] 40 "30 + 10 = 40"
                    Expect.equal result.["d"] 15 "Key only in mapB"

                testCase "is commutative for addition"
                <| fun () ->
                    let mapA = Map.ofList [ ("x", 10) ]
                    let mapB = Map.ofList [ ("x", 3) ]
                    let resultAB = Map.mergeAdd mapA mapB
                    let resultBA = Map.mergeAdd mapB mapA

                    Expect.equal resultAB.["x"] 13 "10 + 3 = 13"
                    Expect.equal resultBA.["x"] 13 "3 + 10 = 13"

                testCase "works with multiple overlapping keys"
                <| fun () ->
                    let mapA =
                        Map.ofList
                            [ ("a", 1)
                              ("b", 2)
                              ("c", 3) ]
                    let mapB =
                        Map.ofList
                            [ ("a", 10)
                              ("b", 20)
                              ("c", 30) ]
                    let result = Map.mergeAdd mapA mapB

                    Expect.equal result.["a"] 11 "1 + 10"
                    Expect.equal result.["b"] 22 "2 + 20"
                    Expect.equal result.["c"] 33 "3 + 30"

                testCase "works with floats"
                <| fun () ->
                    let mapA =
                        Map.ofList
                            [ ("x", 1.5)
                              ("y", 2.5) ]
                    let mapB =
                        Map.ofList
                            [ ("x", 0.5)
                              ("z", 3.5) ]
                    let result = Map.mergeAdd mapA mapB

                    Expect.equal result.["x"] 2.0 "1.5 + 0.5 = 2.0"
                    Expect.equal result.["y"] 2.5 "Key only in mapA"
                    Expect.equal result.["z"] 3.5 "Key only in mapB"

                testCase "handles empty maps"
                <| fun () ->
                    let mapA = Map.ofList [ ("a", 1) ]
                    let mapB: Map<string, int> = Map.empty
                    let result = Map.mergeAdd mapA mapB

                    Expect.equal result mapA "Adding empty map returns original"

                testCase "can be used for frequency counting"
                <| fun () ->
                    let freq1 =
                        Map.ofList
                            [ ("apple", 2)
                              ("banana", 3) ]
                    let freq2 =
                        Map.ofList
                            [ ("apple", 1)
                              ("cherry", 5) ]
                    let combined = Map.mergeAdd freq1 freq2

                    Expect.equal combined.["apple"] 3 "Combined frequency of apple"
                    Expect.equal combined.["banana"] 3 "Frequency from first map"
                    Expect.equal combined.["cherry"] 5 "Frequency from second map" ]

          testList
              "Edge cases"
              [ testCase "handles large maps"
                <| fun () ->
                    let mapA = [ 1..1000 ] |> List.map (fun i -> (i, i)) |> Map.ofList
                    let mapB = [ 500..1500 ] |> List.map (fun i -> (i, i * 2)) |> Map.ofList
                    let result = Map.mergeAdd mapA mapB

                    Expect.equal result.[1] 1 "Key only in mapA"
                    Expect.equal result.[750] 2250 "Overlapping key: 750 + 1500"
                    Expect.equal result.[1500] 3000 "Key only in mapB"

                testCase "preserves map structure"
                <| fun () ->
                    let mapA =
                        Map.ofList
                            [ ("a", 1)
                              ("b", 2) ]
                    let mapB = Map.ofList [ ("c", 3) ]
                    let result = Map.merge mapA mapB

                    Expect.equal (result |> Map.toList |> List.length) 3 "Should have 3 keys"
                    Expect.isTrue (result |> Map.forall (fun _ v -> v > 0)) "All values positive" ] ]
