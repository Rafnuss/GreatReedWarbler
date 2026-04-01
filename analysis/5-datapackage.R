pak::pkg_install("Rafnuss/GeoPressureR@dev")
pak::pkg_install("Rafnuss/GeoLocatoR@dev")


library(GeoPressureR)
library(GeoLocatoR)
library(frictionless)
library(zen4R)
library(readr)

## Publish Data Pacakge
# See https://raphaelnussbaumer.com/GeoLocatoR/articles/create-from-geopressuretemplate.html

# https://zenodo.org/account/settings/applications/tokens/new/
# keyring::key_set_with_value("ZENODO_PAT", password = "32pR233Hj0hx5zemOgTvvdZVgRJ0YWNNZ4PrVBWrTxWec9CcKf81RRXLBgjp")
zenodo <- ZenodoManager$new(token = keyring::key_get(service = "ZENODO_PAT"))

z <- zenodo$getDepositionByConceptDOI("10.5281/zenodo.17084708")
pkg <- zenodo_to_gldp(z)


# Add data
pkg <-  add_gldp_geopressuretemplate(pkg)

# Add tag_comments (or copy pastr from config to tags.csv???)
tags(pkg) <- tags(pkg) %>%
  dplyr::mutate( tag_comments = purrr::map_chr(
    tag_id,
    ~ config::get("tag_comments", .x) %||% NA_character_
  ))

# print(pkg)

# Check package
plot(pkg)
validate_gldp(pkg)

# Write datapackage
dir.create("data/datapackage", showWarnings = FALSE)
write_package(pkg, "data/datapackage/")

# Upload on Zenodo
# https://zenodo.org/uploads/new
# Use the information in datapackage.json to fill the zenodo form.



# Load necessary library
library(dplyr)

# Read the CSV
file_path <- "/Users/yannrime/Library/CloudStorage/OneDrive-Personnel/postdoc/GreatReedWarbler/GreatReedWarbler-main/data/tags2.csv"
df <- read.csv(file_path, sep =";", stringsAsFactors = FALSE)

# Find duplicates in the 'tag_id' column
duplicates <- df %>%
  group_by(tag_id) %>%
  filter(n() > 1)

# View duplicates
print(duplicates)

# Optional: just the duplicate tag_ids
duplicate_ids <- duplicates %>% distinct(tag_id)
print(duplicate_ids)
