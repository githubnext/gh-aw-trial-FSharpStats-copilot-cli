namespace FSharp.Stats.ML

open FSharp.Stats
open System

/// Module for data imputation and missing value filtering
[<Obsolete("This module is deprecated. Use FSharp.Stats.Imputation instead")>]
module Impute =

    module Cleaning =
        
        let calcFractionBy (isMissing) (dataRow:seq<'a>) =     
            Imputation.Cleaning.calcFractionBy isMissing dataRow

    
        let removeAllBy f threshold (data:seq<#seq<'a>>) =
            Imputation.Cleaning.removeAllBy f threshold data

    /// <summary>Imputation by random sampling from the input vector</summary>
    /// <remarks></remarks>
    /// <param name="rnd"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let rnd (rnd:System.Random) : Imputation.VectorBaseImputation<'a> =        
        Imputation.rnd rnd


    /// Imputation by sampling from a gausian normal distribution based on the input vector
    let normal: Imputation.VectorBaseImputation<float> =          
        Imputation.normal


    ///// Imputation by sampling from a gausian normal distribution based on the input vector
    //let normalTruncated :  VectorBaseImputation<float> =          
    //    fun fdata index ->
    //        let mean = Seq.mean fdata
    //        let std  = Seq.stDev fdata
    //        if not(System.Double.IsNaN(mean) || System.Double.IsNaN(std)) then
    //            Distributions.Continuous.Normal.Sample mean std
    //        else
    //            failwithf "Vector needs at least two non-missing value"


    /// <summary>Imputation by k-nearest neighbour</summary>
    /// <remarks></remarks>
    /// <param name="k"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let kNearestImpute k : Imputation.MatrixBaseImputation<float[],float> = 
        Imputation.kNearestImpute k

    /// <summary>Imputes column-wise by vector-based imputation</summary>
    /// <remarks></remarks>
    /// <param name="impute"></param>
    /// <param name="isMissing"></param>
    /// <param name="data"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let imputeColWiseBy (impute: Imputation.VectorBaseImputation<'a>) isMissing (data : seq<#seq<'a>>) =
        Imputation.imputeColWiseBy impute isMissing data


    /// <summary>Imputes row-wise by vector-based imputation</summary>
    /// <remarks></remarks>
    /// <param name="impute"></param>
    /// <param name="isMissing"></param>
    /// <param name="data"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let imputeRowWiseBy (impute: Imputation.VectorBaseImputation<'a>) isMissing (data : seq<#seq<'a>>) =        
        Imputation.imputeRowWiseBy impute isMissing data


    /// <summary>Imputes rows by matrix-based imputation</summary>
    /// <remarks></remarks>
    /// <param name="impute"></param>
    /// <param name="isMissing"></param>
    /// <param name="data"></param>
    /// <returns></returns>
    /// <example>
    /// <code>
    /// </code>
    /// </example>
    let imputeBy (impute: Imputation.MatrixBaseImputation<'a[],'a>) isMissing data =        
        Imputation.imputeBy impute isMissing data
