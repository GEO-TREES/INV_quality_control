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
#' Flag functions typically return a vector of `TRUE`/`FALSE` of the same length as the number of rows in the dataframe input, where `TRUE` indicates that the row has been flagged.
#'
#' Correction functions typically return a dataframe of the same length as the 
#' number of rows in the dataframe input with adjusted values of one or more 
#' variables along with an additional column describing the changes made.
#'
#' @docType package
#' @name treeInvQC
"_PACKAGE"
