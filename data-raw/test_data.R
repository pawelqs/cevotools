
sample_data <- openxlsx::read.xlsx("data-raw/test_data.xlsx") |>
  as_tibble() |>
  rename(row = X1) |>
  pivot_longer(B:E, names_to = "column", values_to = "sample_classification") |>
  filter(!is.na(sample_classification)) |>
  mutate(
    patient_id = "P1",
    coordinates = str_c(column, row),
    sample_id = str_c(patient_id, coordinates, sep = "_"),
    sample_classification = parse_factor(sample_classification, levels = c("NU", "LGIN", "HGIN", "UC"))
  ) |>
  select(patient_id, sample_id, coordinates, row, column, sample_classification)

driver_mutations <- tribble(
  ~sample_id, ~chrom, ~pos, ~VAF, ~gene_symbol,
  "P1_C3",    "chr1", 1,    0.5,  "TP53",
  "P1_C4",    "chr1", 1,    0.3,  "TP53",
  "P1_D3",    "chr1", 1,    0.4,  "TP53",
  "P1_D4",    "chr1", 1,    0.3,  "TP53",
  "P1_C3",    "chr1", 2,    0.5,  "BRCA1",
  "P1_C4",    "chr1", 2,    0.3,  "BRCA1",
  "P1_D3",    "chr1", 2,    0.4,  "BRCA1",
  "P1_E3",    "chr1", 3,    0.2,  "MYC1",
  "P1_E4",    "chr1", 3,    0.1,  "MYC1"
) |>
  mutate(
    ref = "C",
    alt = "G"
  )

test_data <- init_cevodata("Spatial test data") |>
  add_sample_data(sample_data) |>
  add_SNV_data(driver_mutations) |>
  as_spatial_cevodata()

usethis::use_data(test_data, overwrite = TRUE)
