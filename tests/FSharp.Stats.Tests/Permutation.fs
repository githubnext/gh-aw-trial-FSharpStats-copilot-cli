module PermutationTests

open Expecto
open FSharp.Stats

[<Tests>]
let permutationTests =
    testList "Permutation" [
        
        testList "ofArray" [
            testCase "creates permutation from valid array" (fun _ ->
                let arr = [|2; 0; 1|]
                let perm = Permutation.ofArray arr
                Expect.equal (perm 0) 2 "perm 0 should be 2"
                Expect.equal (perm 1) 0 "perm 1 should be 0"
                Expect.equal (perm 2) 1 "perm 2 should be 1"
            )
            
            testCase "creates identity permutation" (fun _ ->
                let arr = [|0; 1; 2; 3|]
                let perm = Permutation.ofArray arr
                Expect.equal (perm 0) 0 "perm 0 should be 0"
                Expect.equal (perm 1) 1 "perm 1 should be 1"
                Expect.equal (perm 2) 2 "perm 2 should be 2"
                Expect.equal (perm 3) 3 "perm 3 should be 3"
            )
            
            testCase "throws on invalid permutation (duplicate)" (fun _ ->
                let arr = [|0; 0; 2|]
                Expect.throws (fun () -> Permutation.ofArray arr |> ignore) "Should throw on duplicate values"
            )
            
            testCase "throws on invalid permutation (out of range)" (fun _ ->
                let arr = [|0; 1; 5|]
                Expect.throws (fun () -> Permutation.ofArray arr |> ignore) "Should throw on out of range values"
            )
            
            testCase "throws on invalid permutation (negative)" (fun _ ->
                let arr = [|0; -1; 2|]
                Expect.throws (fun () -> Permutation.ofArray arr |> ignore) "Should throw on negative values"
            )
        ]
        
        testList "ofPairs" [
            testCase "creates permutation from pairs" (fun _ ->
                let pairs = [(0, 2); (1, 0); (2, 1)]
                let perm = Permutation.ofPairs pairs
                Expect.equal (perm 0) 2 "perm 0 should be 2"
                Expect.equal (perm 1) 0 "perm 1 should be 0"
                Expect.equal (perm 2) 1 "perm 2 should be 1"
            )
            
            testCase "leaves unmapped indices unchanged" (fun _ ->
                let pairs = [(0, 5); (2, 7)]
                let perm = Permutation.ofPairs pairs
                Expect.equal (perm 0) 5 "perm 0 should be 5"
                Expect.equal (perm 1) 1 "perm 1 should be unchanged (1)"
                Expect.equal (perm 2) 7 "perm 2 should be 7"
                Expect.equal (perm 3) 3 "perm 3 should be unchanged (3)"
            )
            
            testCase "handles empty pairs" (fun _ ->
                let pairs = []
                let perm = Permutation.ofPairs pairs
                Expect.equal (perm 0) 0 "perm 0 should be 0"
                Expect.equal (perm 5) 5 "perm 5 should be 5"
            )
        ]
        
        testList "swap" [
            testCase "swaps two positions" (fun _ ->
                let perm = Permutation.swap 1 3
                Expect.equal (perm 0) 0 "perm 0 should be unchanged"
                Expect.equal (perm 1) 3 "perm 1 should be 3"
                Expect.equal (perm 2) 2 "perm 2 should be unchanged"
                Expect.equal (perm 3) 1 "perm 3 should be 1"
                Expect.equal (perm 4) 4 "perm 4 should be unchanged"
            )
            
            testCase "swapping same position is identity" (fun _ ->
                let perm = Permutation.swap 2 2
                Expect.equal (perm 0) 0 "perm 0 should be 0"
                Expect.equal (perm 1) 1 "perm 1 should be 1"
                Expect.equal (perm 2) 2 "perm 2 should be 2"
                Expect.equal (perm 3) 3 "perm 3 should be 3"
            )
            
            testCase "swap is commutative" (fun _ ->
                let perm1 = Permutation.swap 1 4
                let perm2 = Permutation.swap 4 1
                for i in 0..10 do
                    Expect.equal (perm1 i) (perm2 i) $"perm {i} should be equal"
            )
        ]
        
        testList "reversal" [
            testCase "reverses indices for size 5" (fun _ ->
                let perm = Permutation.reversal 5
                Expect.equal (perm 0) 4 "perm 0 should be 4"
                Expect.equal (perm 1) 3 "perm 1 should be 3"
                Expect.equal (perm 2) 2 "perm 2 should be 2"
                Expect.equal (perm 3) 1 "perm 3 should be 1"
                Expect.equal (perm 4) 0 "perm 4 should be 0"
            )
            
            testCase "reversal of size 1 is identity" (fun _ ->
                let perm = Permutation.reversal 1
                Expect.equal (perm 0) 0 "perm 0 should be 0"
            )
            
            testCase "reversal of size 2" (fun _ ->
                let perm = Permutation.reversal 2
                Expect.equal (perm 0) 1 "perm 0 should be 1"
                Expect.equal (perm 1) 0 "perm 1 should be 0"
            )
            
            testCase "throws on non-positive size" (fun _ ->
                Expect.throws (fun () -> Permutation.reversal 0 |> ignore) "Should throw on size 0"
                Expect.throws (fun () -> Permutation.reversal -1 |> ignore) "Should throw on negative size"
            )
            
            testCase "applying reversal twice is identity" (fun _ ->
                let perm = Permutation.reversal 7
                for i in 0..6 do
                    Expect.equal (perm (perm i)) i $"double reversal of {i} should be {i}"
            )
        ]
        
        testList "rotation" [
            testCase "rotates right by positive distance" (fun _ ->
                let perm = Permutation.rotation 5 2
                Expect.equal (perm 0) 2 "perm 0 should be 2"
                Expect.equal (perm 1) 3 "perm 1 should be 3"
                Expect.equal (perm 2) 4 "perm 2 should be 4"
                Expect.equal (perm 3) 0 "perm 3 should wrap to 0"
                Expect.equal (perm 4) 1 "perm 4 should wrap to 1"
            )
            
            testCase "rotates left by negative distance" (fun _ ->
                let perm = Permutation.rotation 5 -2
                Expect.equal (perm 0) 3 "perm 0 should be 3"
                Expect.equal (perm 1) 4 "perm 1 should be 4"
                Expect.equal (perm 2) 0 "perm 2 should wrap to 0"
                Expect.equal (perm 3) 1 "perm 3 should wrap to 1"
                Expect.equal (perm 4) 2 "perm 4 should wrap to 2"
            )
            
            testCase "rotation by 0 is identity" (fun _ ->
                let perm = Permutation.rotation 5 0
                for i in 0..4 do
                    Expect.equal (perm i) i $"perm {i} should be {i}"
            )
            
            testCase "rotation by size wraps completely" (fun _ ->
                // Rotation by size-1 should shift everything by 1
                let perm = Permutation.rotation 4 1
                Expect.equal (perm 0) 1 "perm 0 should be 1"
                Expect.equal (perm 1) 2 "perm 1 should be 2"
                Expect.equal (perm 2) 3 "perm 2 should be 3"
                Expect.equal (perm 3) 0 "perm 3 should be 0"
            )
            
            testCase "throws on non-positive size" (fun _ ->
                Expect.throws (fun () -> Permutation.rotation 0 1 |> ignore) "Should throw on size 0"
                Expect.throws (fun () -> Permutation.rotation -1 1 |> ignore) "Should throw on negative size"
            )
            
            testCase "throws when distance >= size" (fun _ ->
                Expect.throws (fun () -> Permutation.rotation 5 5 |> ignore) "Should throw when distance equals size"
                Expect.throws (fun () -> Permutation.rotation 5 6 |> ignore) "Should throw when distance > size"
            )
            
            testCase "throws when -distance >= size" (fun _ ->
                Expect.throws (fun () -> Permutation.rotation 5 -5 |> ignore) "Should throw when -distance equals size"
                Expect.throws (fun () -> Permutation.rotation 5 -6 |> ignore) "Should throw when -distance > size"
            )
        ]
        
        testList "identity" [
            testCase "returns same index" (fun _ ->
                Expect.equal (Permutation.identity 0) 0 "identity 0 should be 0"
                Expect.equal (Permutation.identity 5) 5 "identity 5 should be 5"
                Expect.equal (Permutation.identity 100) 100 "identity 100 should be 100"
                Expect.equal (Permutation.identity -3) -3 "identity -3 should be -3"
            )
        ]
        
        testList "inverse" [
            testCase "inverts a permutation" (fun _ ->
                let arr = [|2; 0; 1|]
                let perm = Permutation.ofArray arr
                let inv = Permutation.inverse 3 perm
                Expect.equal (inv 0) 1 "inv 0 should be 1"
                Expect.equal (inv 1) 2 "inv 1 should be 2"
                Expect.equal (inv 2) 0 "inv 2 should be 0"
            )
            
            testCase "inverse of identity is identity" (fun _ ->
                let inv = Permutation.inverse 5 Permutation.identity
                for i in 0..4 do
                    Expect.equal (inv i) i $"inv {i} should be {i}"
            )
            
            testCase "inverse of swap is same swap" (fun _ ->
                let perm = Permutation.swap 1 3
                let inv = Permutation.inverse 5 perm
                for i in 0..4 do
                    Expect.equal (perm i) (inv i) $"perm and inv should be equal at {i}"
            )
            
            testCase "inverse of reversal is same reversal" (fun _ ->
                let perm = Permutation.reversal 5
                let inv = Permutation.inverse 5 perm
                for i in 0..4 do
                    Expect.equal (perm i) (inv i) $"reversal inv should equal perm at {i}"
            )
            
            testCase "applying permutation then inverse gives identity" (fun _ ->
                let arr = [|3; 1; 0; 2|]
                let perm = Permutation.ofArray arr
                let inv = Permutation.inverse 4 perm
                for i in 0..3 do
                    Expect.equal (inv (perm i)) i $"inv(perm({i})) should be {i}"
            )
            
            testCase "throws on non-positive size" (fun _ ->
                Expect.throws (fun () -> Permutation.inverse 0 Permutation.identity |> ignore) "Should throw on size 0"
                Expect.throws (fun () -> Permutation.inverse -1 Permutation.identity |> ignore) "Should throw on negative size"
            )
        ]
        
        testList "composition" [
            testCase "composing permutations" (fun _ ->
                // First swap 0 and 1, then swap 1 and 2
                let perm1 = Permutation.swap 0 1
                let perm2 = Permutation.swap 1 2
                let composed = fun k -> perm2 (perm1 k)
                
                // 0 -> perm1 -> 1, then 1 -> perm2 -> 2
                Expect.equal (composed 0) 2 "0 -> 1 -> 2"
                // 1 -> perm1 -> 0, then 0 -> perm2 -> 0
                Expect.equal (composed 1) 0 "1 -> 0 -> 0"
                // 2 -> perm1 -> 2, then 2 -> perm2 -> 1
                Expect.equal (composed 2) 1 "2 -> 2 -> 1"
            )
        ]
    ]
