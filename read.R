#### read.R ####

raw_data <- read_sav("psedii_scrn_ABCDEF.sav")
raw_data <- as_tibble(remove_all_labels(raw_data))

# end