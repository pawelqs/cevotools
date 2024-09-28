verbose::verbose(cevoverse = 0)
data("tcga_brca_test", package = "cevodata")


test_that("plot_CNA_heatmap works", {
  ht <- plot_CNA_heatmap(tcga_brca_test, "seg_mean", verbose = FALSE)
  expect_s4_class(ht, "Heatmap")
})
