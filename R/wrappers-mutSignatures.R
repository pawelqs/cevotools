
#' Annotate mutation context and types for mutation signatures analysis
#' @param snvs snvs tbl
#' @param bsgenome BSGenome object
#' @export
annotate_mutation_contexts <- function(snvs, bsgenome) {
  rlang::check_installed("mutSignatures", reason = "Used to annotate mutations")
  snvs |>
    mutSignatures::attachContext(
      chr_colName = "chrom",
      start_colName = "pos",
      end_colName = "pos",
      nucl_contextN = 3,
      BSGenomeDb = bsgenome
    ) |>
    mutSignatures::removeMismatchMut(
      refMut_colName = "ref",
      context_colName = "context",
      refMut_format = "N"
    ) |>
    mutSignatures::attachMutType(
      ref_colName = "ref",
      var_colName = "alt",
      context_colName = "context"
    )
}


#' Get matrix of per sample mutation types for mutation signatures analysis
#' @param snvs annotated snvs tbl
#' @return wide tbl
#' @export
count_mutation_types <- function(snvs) {
  rlang::check_installed("mutSignatures", reason = "Used to count mutations")
  counts <- mutSignatures::countMutTypes(
    snvs,
    mutType_colName = "mutType",
    sample_colName = "sample_id"
  )

  counts_mat <- counts@counts
  rownames(counts_mat) <- counts@mutTypes$mutTypes
  colnames(counts_mat) <- counts@sampleId$ID

  counts_mat |>
    rownames_to_column("MutationType")

}

