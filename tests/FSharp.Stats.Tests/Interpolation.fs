module InterpolationTests

open Expecto
open FSharp.Stats
open FSharp.Stats.Interpolation

open TestExtensions

[<Tests>]
let cubicInterpolationTests =
    let t =
        vector
            [ 0.0
              1.0
              2.0
              3.0 ]
    let y =
        vector
            [ 187.6
              185.7
              193.7
              197.0 ]
    let tt =
        vector
            [ 0.0
              0.25
              0.5
              0.75
              1.
              1.25
              1.5
              1.75
              2.
              2.25
              2.5
              2.75
              3.0 ]

    let u =
        vector
            [ 1.0
              4.0
              9.0
              16.0 ]
    let t2 =
        vector
            [ 1.0
              2.0
              3.0
              4.0 ]

    testList
        "Interpolation.CubicSpline"
        [ testCase "Natural Cubic Spline"
          <| fun () ->
              //Verifies that the fitted point match the expectred fittied points
              //https://columbiaeconomics.com/2010/01/20/how-economists-convert-quarterly-data-into-monthly-cubic-spline-interpolation/comment-page-1/
              let coefficientsSpline = CubicSpline.interpolate CubicSpline.Natural t y
              let fitOutPut = tt |> Vector.map (CubicSpline.predict coefficientsSpline)
              let expectedValues =
                  vector
                      [ 187.6
                        186.4328125
                        185.5425
                        185.2059375
                        185.7
                        187.179375
                        189.31
                        191.635625
                        193.7
                        195.1528125
                        196.0675
                        196.6234375
                        197.0 ]
              TestExtensions.sequenceEqual
                  Accuracy.low
                  expectedValues
                  fitOutPut
                  "Fitted Values and Expected Output should be equal (double precision)"

          testCase "Quadratic Cubic Spline"
          <| fun () ->
              let coefficientsQuadraticSpline = CubicSpline.interpolate CubicSpline.Quadratic t2 u
              let fittingFunc x =
                  CubicSpline.predictWithinRange coefficientsQuadraticSpline x
              Expect.floatClose Accuracy.high (fittingFunc 1.5) 2.25 "Fitted Value should be equal (double precision)"
              Expect.floatClose Accuracy.high (fittingFunc 2.5) 6.25 "Fitted Value should be equal (double precision)"
              Expect.floatClose Accuracy.high (fittingFunc 3.5) 12.25 "Fitted Value should be equal (double precision)"

          let seriesx =
              [| 20.15
                 24.41
                 28.78 |]
              |> Array.sort
              |> vector
          let seriesy =
              [| 0.367
                 0.591
                 0.796 |]
              |> Array.sort
              |> vector
          testCase "Parabolic Cubic Interpolation"
          <| fun () ->
              //http://support.ptc.com/help/mathcad/en/index.html#page/PTC_Mathcad_Help%2Fexample_cubic_spline_interpolation.html%23
              let coeffParabolic = CubicSpline.interpolate CubicSpline.Parabolic seriesx seriesy
              let fittingFuncParabolic x = CubicSpline.predict coeffParabolic x

              let genrateX = vector [ 20.0 .. 25.0 ]
              let interpParabolic = genrateX |> Vector.map fittingFuncParabolic
              let parabolicSndDeriv x =
                  CubicSpline.getSecondDerivative coeffParabolic x

              Expect.floatClose
                  Accuracy.high
                  (parabolicSndDeriv interpParabolic.[0])
                  (parabolicSndDeriv interpParabolic.[1])
                  "the second derivative at the first and second points should be equal (double precision)" ]

[<Tests>]
let akimaInterpolationTests =
    let t =
        vector
            [ 0.0
              1.0
              2.0
              3.0 ]
    let y =
        vector
            [ 187.6
              185.7
              193.7
              197.0 ]
    let tt =
        vector
            [ 0.0
              0.25
              0.5
              0.75
              1.
              1.25
              1.5
              1.75
              2.
              2.25
              2.5
              2.75
              3.0 ]

    let u =
        vector
            [ 1.0
              4.0
              9.0
              16.0 ]
    let t2 =
        vector
            [ 1.0
              2.0
              3.0
              4.0 ]

    testList
        "Interpolation.CubicSpline"
        [

          let values =
              [| 0.0
                 2.0
                 1.0
                 3.0
                 2.0
                 6.0
                 5.5
                 5.5
                 2.7
                 5.1
                 3.0 |]
          let time = [| 0.0 .. 10.0 |]
          testCase "Akima Interpolation"
          <| fun () ->
              let splineCoefsAkima = Akima.interpolate time values
              let fittingFuncAkima x = Akima.predict splineCoefsAkima x
              Expect.floatClose
                  Accuracy.high
                  (fittingFuncAkima 0.5)
                  1.375
                  "Fitted Value should be equal (double precision)"
              Expect.floatClose
                  Accuracy.high
                  (fittingFuncAkima 1.0)
                  2.0
                  "Fitted Value should be equal (double precision)"
              Expect.floatClose
                  Accuracy.high
                  (fittingFuncAkima 1.5)
                  1.5
                  "Fitted Value  should be equal (double precision)"
              Expect.floatClose
                  Accuracy.high
                  (fittingFuncAkima 2.5)
                  1.953125
                  "Fitted Value should be equal (double precision)"
              Expect.floatClose
                  Accuracy.high
                  (fittingFuncAkima 3.5)
                  2.484375
                  "Fitted Value should be equal (double precision)"
              Expect.floatClose
                  Accuracy.medium
                  (fittingFuncAkima 4.5)
                  4.136363
                  "Fitted Value should be equal (double precision)"

          ]

[<Tests>]
let polynomialInterpolationTests =
    testList
        "Interpolation.Polynomial"
        [ let datax =
              vector
                  [ 301.0
                    306.0
                    318.0
                    332.0
                    333.0 ]
          let datay =
              vector
                  [ 0.02
                    0.2
                    -0.04
                    0.06
                    0.17 ]

          testCase "Polynomial Interpolation"
          <| fun () ->
              //http://support.ptc.com/help/mathcad/en/index.html#page/PTC_Mathcad_Help%2Fexample_polynomial_interpolation.html%23wwID0E3LVS
              let coeffs = Polynomial.interpolate datax datay
              let expectedCoeffs =
                  [ 18489.1150794045
                    -249.9950165
                    1.2620688143
                    -0.0028205075
                    0.0000023552 ]
              let polyInterpFit x = Polynomial.predict coeffs x
              Expect.floatClose
                  Accuracy.high
                  (polyInterpFit 328.0)
                  -0.1894337636
                  "Fitted Value should be equal (double precision)"
              TestExtensions.sequenceEqual
                  (Accuracy.high)
                  (datax |> Seq.map polyInterpFit)
                  datay
                  "Fitted Value should be equal (double precision)"
              TestExtensions.sequenceEqual
                  (Accuracy.high)
                  coeffs.C0_CX
                  expectedCoeffs
                  "Coefficients should be equal (double precision)" ]

[<Tests>]
let BezierInterpolationTests =
    testList
        "Interpolation.Bezier"
        [

          testCase "Bezier Interpolation of degree 1"
          <| fun () ->
              // Without control point, this is just linear interpolation
              let p0 =
                  vector
                      [| 1.
                         1.
                         1. |] //point 0 that should be traversed
              let p1 =
                  vector
                      [| 3.
                         2.
                         0. |] //point 1 that should be traversed
              let data =
                  [| p0
                     p1 |]
              let interpolate = Bezier.interpolate data
              let expectedMiddle = p0 + 0.5 * (p1 - p0)
              TestExtensions.sequenceEqual
                  (Accuracy.high)
                  (interpolate 0.)
                  p0
                  "Initial point should be equal (double precision)"
              TestExtensions.sequenceEqual
                  (Accuracy.high)
                  (interpolate 0.5)
                  expectedMiddle
                  "Middle point should be equal (double precision)"
              TestExtensions.sequenceEqual
                  (Accuracy.high)
                  (interpolate 1.)
                  p1
                  "Final point should be equal (double precision)"

          testCase "Bezier Interpolation of degree 2"
          <| fun () ->
              let p0 =
                  vector
                      [| 1.
                         1.
                         1. |] //point 0 that should be traversed
              let c0 =
                  vector
                      [| 1.1
                         2.1
                         2. |] //control point 0
              let p1 =
                  vector
                      [| 3.
                         2.
                         0. |] //point 1 that should be traversed
              let data =
                  [| p0
                     c0
                     p1 |]
              let interpolate = Bezier.interpolate data
              let a = p0 + 0.5 * (c0 - p0)
              let b = c0 + 0.5 * (p1 - c0)
              let expectedMiddle = a + 0.5 * (b - a)
              TestExtensions.sequenceEqual
                  (Accuracy.high)
                  (interpolate 0.)
                  p0
                  "Initial point should be equal (double precision)"
              TestExtensions.sequenceEqual
                  (Accuracy.high)
                  (interpolate 0.5)
                  expectedMiddle
                  "Middle point should be equal (double precision)"
              TestExtensions.sequenceEqual
                  (Accuracy.high)
                  (interpolate 1.)
                  p1
                  "Final point should be equal (double precision)"

          testCase "Bezier Interpolation of degree 3"
          <| fun () ->
              let p0 =
                  vector
                      [| 1.
                         1.
                         1. |] //point 0 that should be traversed
              let c0 =
                  vector
                      [| 1.1
                         2.1
                         2. |] //control point 0
              let c1 =
                  vector
                      [| 3.8
                         1.6
                         1.4 |] //control point 1
              let p1 =
                  vector
                      [| 3.
                         2.
                         0. |] //point 1 that should be traversed
              let data =
                  [| p0
                     c0
                     c1
                     p1 |]
              let interpolate = Bezier.interpolate data
              let a = p0 + 0.5 * (c0 - p0)
              let b = c0 + 0.5 * (c1 - c0)
              let c = c1 + 0.5 * (p1 - c1)
              let d = a + 0.5 * (b - a)
              let e = b + 0.5 * (c - b)
              let expectedMiddle = d + 0.5 * (e - d)
              TestExtensions.sequenceEqual
                  (Accuracy.high)
                  (interpolate 0.)
                  p0
                  "Initial point should be equal (double precision)"
              TestExtensions.sequenceEqual
                  (Accuracy.high)
                  (interpolate 0.5)
                  expectedMiddle
                  "Middle point should be equal (double precision)"
              TestExtensions.sequenceEqual
                  (Accuracy.high)
                  (interpolate 1.)
                  p1
                  "Final point should be equal (double precision)"

          ]

[<Tests>]
let linearSplineTests =
    testList
        "Interpolation.LinearSpline"
        [ testCase "interpolateSorted creates linear spline from sorted data"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2. |]
              let yData =
                  [| 0.
                     1.
                     2. |]
              let coef = LinearSpline.interpolateSorted xData yData
              let result = coef.Predict 0.5
              Expect.floatClose Accuracy.high result 0.5 "Should interpolate linearly"

          testCase "interpolate handles unsorted data"
          <| fun () ->
              let xData =
                  [| 2.
                     0.
                     1. |]
              let yData =
                  [| 2.
                     0.
                     1. |]
              let coef = LinearSpline.interpolate xData yData
              let result = coef.Predict 0.5
              Expect.floatClose Accuracy.high result 0.5 "Should interpolate after sorting"

          testCase "interpolateInplace sorts in place"
          <| fun () ->
              let xData =
                  [| 2.
                     0.
                     1. |]
              let yData =
                  [| 2.
                     0.
                     1. |]
              let coef = LinearSpline.interpolateInplace xData yData
              Expect.equal xData.[0] 0. "Should sort x data in place"
              Expect.equal yData.[0] 0. "Should sort y data in place"

          testCase "predict interpolates between knots"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2. |]
              let yData =
                  [| 0.
                     2.
                     4. |]
              let coef = LinearSpline.interpolateSorted xData yData
              let result = LinearSpline.predict coef 1.5
              Expect.floatClose Accuracy.high result 3.0 "Should predict 3.0"

          testCase "differentiate returns slope"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2. |]
              let yData =
                  [| 0.
                     2.
                     4. |]
              let coef = LinearSpline.interpolateSorted xData yData
              let slope = LinearSpline.differentiate coef 0.5
              Expect.floatClose Accuracy.high slope 2.0 "Slope should be 2.0"

          testCase "Differentiate member returns derivative coefficients"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2. |]
              let yData =
                  [| 0.
                     2.
                     4. |]
              let coef = LinearSpline.interpolateSorted xData yData
              let deriv = coef.Differentiate()
              let result = deriv.Predict 0.5
              Expect.floatClose Accuracy.high result 2.0 "Derivative should be 2.0" ]

[<Tests>]
let stepTests =
    testList
        "Interpolation.Step"
        [ testCase "interpolateSorted creates step function"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2. |]
              let yData =
                  [| 1.
                     2.
                     3. |]
              let coef = Step.interpolateSorted xData yData
              let result = coef.Predict 0.5
              Expect.floatClose Accuracy.high result 1.0 "Should return step value"

          testCase "interpolate handles unsorted data"
          <| fun () ->
              let xData =
                  [| 2.
                     0.
                     1. |]
              let yData =
                  [| 3.
                     1.
                     2. |]
              let coef = Step.interpolate xData yData
              let result = coef.Predict 0.5
              Expect.floatClose Accuracy.high result 1.0 "Should return step value after sorting"

          testCase "predict returns constant value in interval"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2. |]
              let yData =
                  [| 1.
                     2.
                     3. |]
              let coef = Step.interpolateSorted xData yData
              let result = Step.predict coef 1.5
              Expect.floatClose Accuracy.high result 2.0 "Should return 2.0"

          testCase "differentiate always returns zero"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2. |]
              let yData =
                  [| 1.
                     2.
                     3. |]
              let coef = Step.interpolateSorted xData yData
              let result = Step.differentiate coef 0.5
              Expect.floatClose Accuracy.high result 0.0 "Derivative of step function is 0"

          testCase "Differentiate member returns zero derivative"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2. |]
              let yData =
                  [| 1.
                     2.
                     3. |]
              let coef = Step.interpolateSorted xData yData
              let deriv = coef.Differentiate()
              let result = deriv.Predict 0.5
              Expect.floatClose Accuracy.high result 0.0 "Step derivative should be 0" ]

[<Tests>]
let polynomialMemberTests =
    testList
        "Interpolation.Polynomial members"
        [ testCase "Predict member interpolates"
          <| fun () ->
              let xData =
                  vector
                      [| 0.
                         1.
                         2. |]
              let yData =
                  vector
                      [| 0.
                         1.
                         4. |]
              let coef = Polynomial.interpolate xData yData
              let result = coef.Predict 1.5
              Expect.floatClose Accuracy.high result 2.25 "Should interpolate quadratically"

          testCase "Differentiate member computes derivative"
          <| fun () ->
              let xData =
                  vector
                      [| 0.
                         1.
                         2. |]
              let yData =
                  vector
                      [| 0.
                         1.
                         4. |]
              let coef = Polynomial.interpolate xData yData
              let deriv = coef.Differentiate 1
              let slope = deriv.Predict 1.0
              Expect.floatClose Accuracy.high slope 2.0 "First derivative at x=1 should be 2"

          testCase "GetDerivative member computes derivative at point"
          <| fun () ->
              let xData =
                  vector
                      [| 0.
                         1.
                         2. |]
              let yData =
                  vector
                      [| 0.
                         1.
                         4. |]
              let coef = Polynomial.interpolate xData yData
              let slope = coef.GetDerivative 1 1.0
              Expect.floatClose Accuracy.high slope 2.0 "First derivative should be 2"

          testCase "Integrate member creates integral polynomial"
          <| fun () ->
              let xData =
                  vector
                      [| 0.
                         1.
                         2. |]
              let yData =
                  vector
                      [| 1.
                         1.
                         1. |]
              let coef = Polynomial.interpolate xData yData
              let integral = coef.Integrate()
              let area = integral.Predict 2.0 - integral.Predict 0.0
              Expect.floatClose Accuracy.high area 2.0 "Integral from 0 to 2 should be 2"

          testCase "GetIntegralBetween member calculates area"
          <| fun () ->
              let xData =
                  vector
                      [| 0.
                         1.
                         2. |]
              let yData =
                  vector
                      [| 1.
                         1.
                         1. |]
              let coef = Polynomial.interpolate xData yData
              let area = coef.GetIntegralBetween 0.0 2.0
              Expect.floatClose Accuracy.high area 2.0 "Area should be 2" ]

[<Tests>]
let hermiteTests =
    testList
        "Interpolation.CubicSpline.Hermite"
        [ testCase "interpolate creates hermite spline"
          <| fun () ->
              let xData =
                  vector
                      [| 0.
                         1.
                         2.
                         3. |]
              let yData =
                  vector
                      [| 0.
                         1.
                         0.
                         1. |]
              let coef = CubicSpline.Hermite.interpolate xData yData
              let result = coef.Predict 1.5
              Expect.isTrue (result >= 0.0 && result <= 1.0) "Result should be in range"

          testCase "interpolateSorted handles sorted data"
          <| fun () ->
              let xData =
                  vector
                      [| 0.
                         1.
                         2.
                         3. |]
              let yData =
                  vector
                      [| 0.
                         1.
                         2.
                         3. |]
              let coef = CubicSpline.Hermite.interpolateSorted xData yData
              let result = coef.Predict 1.5
              Expect.floatClose Accuracy.high result 1.5 "Should interpolate linearly"

          testCase "interpolateWithSlopes uses provided slopes"
          <| fun () ->
              let xData =
                  vector
                      [| 0.
                         1.
                         2. |]
              let yData =
                  vector
                      [| 0.
                         1.
                         2. |]
              let slopes =
                  vector
                      [| 1.
                         1.
                         1. |]
              let coef = CubicSpline.Hermite.interpolateWithSlopes xData yData slopes
              let result = coef.Predict 0.5
              Expect.floatClose Accuracy.high result 0.5 "Should interpolate with given slopes"

          testCase "predict interpolates at point"
          <| fun () ->
              let xData =
                  vector
                      [| 0.
                         1.
                         2. |]
              let yData =
                  vector
                      [| 0.
                         1.
                         0. |]
              let coef = CubicSpline.Hermite.interpolate xData yData
              let result = CubicSpline.Hermite.predict coef 1.0
              Expect.floatClose Accuracy.high result 1.0 "Should match knot value"

          testCase "interpolatePreserveMonotonicity maintains monotonicity"
          <| fun () ->
              let xData =
                  vector
                      [| 0.
                         1.
                         2.
                         3.
                         4. |]
              let yData =
                  vector
                      [| 0.
                         1.
                         2.
                         3.
                         4. |]
              let coef = CubicSpline.Hermite.interpolatePreserveMonotonicity xData yData
              let v1 = coef.Predict 0.5
              let v2 = coef.Predict 1.5
              let v3 = coef.Predict 2.5
              Expect.isTrue (v1 < v2 && v2 < v3) "Should preserve monotonicity" ]

[<Tests>]
let approximationTests =
    testList
        "Interpolation.Approximation"
        [ testCase "regularizeValues handles duplicates"
          <| fun () ->
              let xData =
                  [ 1.0
                    1.0
                    2.0
                    3.0 ]
              let yData =
                  [ 1.0
                    2.0
                    3.0
                    4.0 ]
              let result = Approximation.regularizeValues xData yData Seq.average |> Seq.toList
              Expect.equal result.Length 3 "Should have 3 unique x values"
              Expect.equal (fst result.[0]) 1.0 "First x should be 1.0"
              Expect.floatClose Accuracy.high (snd result.[0]) 1.5 "First y should be average of 1.0 and 2.0"

          testCase "approx interpolates at given points"
          <| fun () ->
              let xData =
                  [ 0.0
                    1.0
                    2.0 ]
              let yData =
                  [ 0.0
                    1.0
                    2.0 ]
              let v =
                  [ 0.5
                    1.5 ]
              let result = Approximation.approx xData yData v Seq.average |> Seq.toList
              Expect.floatClose Accuracy.high result.[0] 0.5 "Should interpolate to 0.5"
              Expect.floatClose Accuracy.high result.[1] 1.5 "Should interpolate to 1.5"

          testCase "chebyshevNodes creates proper spacing"
          <| fun () ->
              let interval = Interval.CreateClosed<float>(0., 1.)
              let nodes = Approximation.chebyshevNodes interval 5
              Expect.equal (Vector.length nodes) 5 "Should create 5 nodes"
              Expect.isTrue (nodes.[0] >= 0.0 && nodes.[0] <= 1.0) "Nodes should be within interval"

          testCase "equalNodes creates equal spacing"
          <| fun () ->
              let interval = Interval.CreateClosed<float>(0., 4.)
              let nodes = Approximation.equalNodes interval 5
              Expect.equal (Vector.length nodes) 5 "Should create 5 nodes"
              Expect.floatClose Accuracy.high nodes.[1] 1.0 "Second node should be 1.0"
              Expect.floatClose Accuracy.high nodes.[2] 2.0 "Third node should be 2.0" ]

[<Tests>]
let approximationClassTests =
    testList
        "Interpolation.Approximation class"
        [ testCase "approxWithPolynomial using equal spacing"
          <| fun () ->
              let f = fun x -> x * x
              let interval = Interval.CreateClosed<float>(0., 2.)
              let coef =
                  Approximation.approxWithPolynomial (f, interval, 3, Approximation.Spacing.Equally)
              let result = coef.Predict 1.0
              Expect.floatClose Accuracy.medium result 1.0 "Should approximate x^2 at x=1"

          testCase "approxWithPolynomial using Chebyshev spacing"
          <| fun () ->
              let f = fun x -> x * x
              let interval = Interval.CreateClosed<float>(0., 2.)
              let coef =
                  Approximation.approxWithPolynomial (f, interval, 3, Approximation.Spacing.Chebyshev)
              let result = coef.Predict 1.0
              Expect.floatClose Accuracy.medium result 1.0 "Should approximate x^2 at x=1"

          testCase "approxWithPolynomialFromValues using equal spacing"
          <| fun () ->
              let xData =
                  [ 0.0
                    1.0
                    2.0 ]
              let yData =
                  [ 0.0
                    1.0
                    4.0 ]
              let coef =
                  Approximation.approxWithPolynomialFromValues (xData, yData, 3, Approximation.Spacing.Equally)
              let result = coef.Predict 1.5
              Expect.floatClose Accuracy.medium result 2.25 "Should approximate quadratic"

          testCase "approxWithPolynomialFromValues using Chebyshev spacing"
          <| fun () ->
              let xData =
                  [ 0.0
                    1.0
                    2.0 ]
              let yData =
                  [ 0.0
                    1.0
                    4.0 ]
              let coef =
                  Approximation.approxWithPolynomialFromValues (xData, yData, 3, Approximation.Spacing.Chebyshev)
              let result = coef.Predict 1.0
              Expect.floatClose Accuracy.medium result 1.0 "Should approximate at x=1" ]

[<Tests>]
let interpolationTypeTests =
    testList
        "Interpolation type"
        [ testCase "interpolate with LinearSpline method"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2. |]
              let yData =
                  [| 0.
                     1.
                     2. |]
              let coef =
                  Interpolation.interpolate (xData, yData, InterpolationMethod.LinearSpline)
              let func = Interpolation.predict (coef)
              let result = func 0.5
              Expect.floatClose Accuracy.high result 0.5 "Should interpolate linearly"

          testCase "interpolate with Polynomial method"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2. |]
              let yData =
                  [| 0.
                     1.
                     4. |]
              let coef = Interpolation.interpolate (xData, yData, InterpolationMethod.Polynomial)
              let func = Interpolation.predict (coef)
              let result = func 1.5
              Expect.floatClose Accuracy.high result 2.25 "Should interpolate quadratically"

          testCase "interpolate with Step method"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2. |]
              let yData =
                  [| 1.
                     2.
                     3. |]
              let coef = Interpolation.interpolate (xData, yData, InterpolationMethod.Step)
              let func = Interpolation.predict (coef)
              let result = func 0.5
              Expect.floatClose Accuracy.high result 1.0 "Should use step interpolation"

          testCase "interpolate with CubicSpline Periodic"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2.
                     3. |]
              let yData =
                  [| 0.
                     1.
                     0.
                     0. |]
              let coef =
                  Interpolation.interpolate (
                      xData,
                      yData,
                      InterpolationMethod.CubicSpline CubicSpline.BoundaryCondition.Periodic
                  )
              let func = Interpolation.predict (coef)
              let result = func 1.0
              Expect.floatClose Accuracy.high result 1.0 "Should match knot"

          testCase "interpolate with CubicSpline Parabolic"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2.
                     3. |]
              let yData =
                  [| 0.
                     1.
                     0.
                     1. |]
              let coef =
                  Interpolation.interpolate (
                      xData,
                      yData,
                      InterpolationMethod.CubicSpline CubicSpline.BoundaryCondition.Parabolic
                  )
              let func = Interpolation.predict (coef)
              let result = func 1.0
              Expect.floatClose Accuracy.high result 1.0 "Should match knot"

          testCase "interpolate with CubicSpline NotAKnot"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2.
                     3. |]
              let yData =
                  [| 0.
                     1.
                     0.
                     1. |]
              let coef =
                  Interpolation.interpolate (
                      xData,
                      yData,
                      InterpolationMethod.CubicSpline CubicSpline.BoundaryCondition.NotAKnot
                  )
              let func = Interpolation.predict (coef)
              let result = func 1.0
              Expect.floatClose Accuracy.high result 1.0 "Should match knot"

          testCase "interpolate with CubicSpline Clamped"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2.
                     3. |]
              let yData =
                  [| 0.
                     1.
                     2.
                     3. |]
              let coef =
                  Interpolation.interpolate (
                      xData,
                      yData,
                      InterpolationMethod.CubicSpline(CubicSpline.BoundaryCondition.Clamped(1.0, 1.0))
                  )
              let func = Interpolation.predict (coef)
              let result = func 1.5
              Expect.floatClose Accuracy.medium result 1.5 "Should interpolate"

          testCase "interpolate with CubicSpline SecondDerivative"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2.
                     3. |]
              let yData =
                  [| 0.
                     1.
                     4.
                     9. |]
              let coef =
                  Interpolation.interpolate (
                      xData,
                      yData,
                      InterpolationMethod.CubicSpline(CubicSpline.BoundaryCondition.SecondDerivative(2.0, 2.0))
                  )
              let func = Interpolation.predict (coef)
              let result = func 1.5
              Expect.isTrue (result > 0.0) "Should interpolate"

          testCase "interpolate with AkimaSubSpline"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2.
                     3.
                     4. |]
              let yData =
                  [| 0.
                     1.
                     0.
                     1.
                     0. |]
              let coef =
                  Interpolation.interpolate (xData, yData, InterpolationMethod.AkimaSubSpline)
              let func = Interpolation.predict (coef)
              let result = func 2.0
              Expect.floatClose Accuracy.high result 0.0 "Should match knot"

          testCase "interpolate with HermiteSpline CSpline"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2.
                     3. |]
              let yData =
                  [| 0.
                     1.
                     2.
                     3. |]
              let coef =
                  Interpolation.interpolate (xData, yData, InterpolationMethod.HermiteSpline HermiteMethod.CSpline)
              let func = Interpolation.predict (coef)
              let result = func 1.5
              Expect.floatClose Accuracy.high result 1.5 "Should interpolate linearly"

          testCase "interpolate with HermiteSpline WithSlopes"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2. |]
              let yData =
                  [| 0.
                     1.
                     2. |]
              let slopes =
                  vector
                      [| 1.
                         1.
                         1. |]
              let coef =
                  Interpolation.interpolate (
                      xData,
                      yData,
                      InterpolationMethod.HermiteSpline(HermiteMethod.WithSlopes slopes)
                  )
              let func = Interpolation.predict (coef)
              let result = func 0.5
              Expect.floatClose Accuracy.high result 0.5 "Should interpolate"

          testCase "interpolate with HermiteSpline PreserveMonotonicity"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2.
                     3.
                     4. |]
              let yData =
                  [| 0.
                     1.
                     2.
                     3.
                     4. |]
              let coef =
                  Interpolation.interpolate (
                      xData,
                      yData,
                      InterpolationMethod.HermiteSpline HermiteMethod.PreserveMonotonicity
                  )
              let func = Interpolation.predict (coef)
              let v1 = func 0.5
              let v2 = func 1.5
              Expect.isTrue (v1 < v2) "Should preserve monotonicity"

          testCase "getFirstDerivative with LinearSpline"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2. |]
              let yData =
                  [| 0.
                     2.
                     4. |]
              let coef =
                  Interpolation.interpolate (xData, yData, InterpolationMethod.LinearSpline)
              let func = Interpolation.getFirstDerivative (coef)
              let result = func 0.5
              Expect.floatClose Accuracy.high result 2.0 "Derivative should be 2.0"

          testCase "getFirstDerivative with Polynomial"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2. |]
              let yData =
                  [| 0.
                     1.
                     4. |]
              let coef = Interpolation.interpolate (xData, yData, InterpolationMethod.Polynomial)
              let func = Interpolation.getFirstDerivative (coef)
              let result = func 1.0
              Expect.floatClose Accuracy.high result 2.0 "Derivative should be 2.0"

          testCase "getSecondDerivative with Polynomial"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2. |]
              let yData =
                  [| 0.
                     1.
                     4. |]
              let coef = Interpolation.interpolate (xData, yData, InterpolationMethod.Polynomial)
              let func = Interpolation.getSecondDerivative (coef)
              let result = func 1.0
              Expect.floatClose Accuracy.high result 2.0 "Second derivative should be 2.0"

          testCase "getIntegralBetween with Polynomial"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2. |]
              let yData =
                  [| 1.
                     1.
                     1. |]
              let coef = Interpolation.interpolate (xData, yData, InterpolationMethod.Polynomial)
              let area = Interpolation.getIntegralBetween (coef, 0.0, 2.0)
              Expect.floatClose Accuracy.high area 2.0 "Area should be 2.0"

          testCase "getIntegralBetween with AkimaSubSpline"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2.
                     3.
                     4. |]
              let yData =
                  [| 1.
                     1.
                     1.
                     1.
                     1. |]
              let coef =
                  Interpolation.interpolate (xData, yData, InterpolationMethod.AkimaSubSpline)
              let area = Interpolation.getIntegralBetween (coef, 0.0, 4.0)
              Expect.floatClose Accuracy.medium area 4.0 "Area should be approximately 4.0"

          testCase "InterpolationCoefficients getter methods"
          <| fun () ->
              let xData =
                  [| 0.
                     1.
                     2. |]
              let yData =
                  [| 0.
                     1.
                     2. |]

              let stepCoef = Interpolation.interpolate (xData, yData, InterpolationMethod.Step)
              let _ = stepCoef.GetStepCoef()

              let linearCoef =
                  Interpolation.interpolate (xData, yData, InterpolationMethod.LinearSpline)
              let _ = linearCoef.GetLinearSplineCoef()

              let polyCoef =
                  Interpolation.interpolate (xData, yData, InterpolationMethod.Polynomial)
              let _ = polyCoef.GetPolynomialCoef()

              let cubicCoef =
                  Interpolation.interpolate (
                      [| 0.
                         1.
                         2.
                         3. |],
                      [| 0.
                         1.
                         0.
                         1. |],
                      InterpolationMethod.CubicSpline CubicSpline.BoundaryCondition.Natural
                  )
              let _ = cubicCoef.GetCubicSplineCoef()

              let akimaCoef =
                  Interpolation.interpolate (
                      [| 0.
                         1.
                         2.
                         3.
                         4. |],
                      [| 0.
                         1.
                         0.
                         1.
                         0. |],
                      InterpolationMethod.AkimaSubSpline
                  )
              let _ = akimaCoef.GetAkimaSubSplineCoef()

              let hermiteCoef =
                  Interpolation.interpolate (
                      [| 0.
                         1.
                         2.
                         3. |],
                      [| 0.
                         1.
                         2.
                         3. |],
                      InterpolationMethod.HermiteSpline HermiteMethod.CSpline
                  )
              let _ = hermiteCoef.GetHermiteSplineCoef()

              Expect.isTrue true "All getter methods should work" ]
