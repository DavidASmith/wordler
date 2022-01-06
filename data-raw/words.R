library(qdapDictionaries)

# Get all five letter words from qdap DICTIONARY --------------------------

qdap_dict <- DICTIONARY$word
qdap_dict <- qdap_dict[nchar(qdap_dict) == 5]
qdap_dict <- toupper(qdap_dict)
qdap_dict


# Add to package external data --------------------------------------------

usethis::use_data(qdap_dict, overwrite = TRUE)
