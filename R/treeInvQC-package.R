#' treeInvQC: Quality control for tree inventory data in plots 
#'
#' A general suite of tools for quality control of single- and
#' multi-census tree inventory data in plots. Includes a built-in Quarto notebook
#' which leverages the package functions to provide a reproducible QC workflow.
#'
#' Functions are split into two main groups, those that __flag__ potential data 
#' quality issues (`flag*()`), and others which attempt to __correct__ these 
#' issues (`correct*()`). 
#' 
#' Flag functions return a dataframe of the records which have been flagged,
#' with the ID columns needed to identify each record, e.g. measurement, 
#' stem, tree, census, or plot IDs, and the values which were checked. If no 
#' records are flagged, the dataframe has no rows.
#'
#' Correction functions return a dataframe of the records which have been 
#' corrected, with the ID columns needed to identify each record, the 
#' original and corrected values, and a column describing the correction.
#'
#' Simple checks, e.g. missing values or values outside a range, are written 
#' as plain R code in the notebook, so that the QA/QC process is transparent.
#'
#' @docType package
#' @name treeInvQC
"_PACKAGE"
