# This code produces the ASCII decimal codes for NULL and
# SUB characters to be removed from text files during sourcing.
nonPrintables <- c(0,26)
devtools::use_data(nonPrintables, internal = TRUE, overwrite = TRUE)
