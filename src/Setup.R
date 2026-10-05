# Set up #### 

# R 4.5.2
# Updated August 8 2026

# Install and load packages, define custom functions



# Packages #### 

library(tidyverse)
library(stringi)
library(purrr)
library(conflicted)
library(here)
library(lubridate)
library(shiny)
library(DT)
library(rsconnect)


# Set preferences #### 

# Preferred commands
conflicts_prefer(here::here)
conflicts_prefer(dplyr::filter)

# Disable scientific notation
options(scipen = 999)



# Elongate scouting reports #### 

elongate_reports <- function(df){
  # Detect all datetime columns (POSIXct / POSIXlt) and protect them
  posix_cols <- names(df)[vapply(df, function(x) inherits(x, c("POSIXct", "POSIXlt")), logical(1))]
  # Only replace NA in character-ish cols
  df <- df %>%
    dplyr::mutate(
      dplyr::across(where(is.factor), as.character),
      dplyr::across(
        .cols = dplyr::where(is.character) & !dplyr::all_of(posix_cols),
        ~ tidyr::replace_na(.x, "")
      )
    ) %>%
    dplyr::distinct()
  # Get base stressor columns (abiotic1, disease2, etc.)
  base_cols <- grep("^(abiotic|disease|pest|weed)[0-9]+$", names(df), value = TRUE)
  # Combine stressor + severity + photo per slot (by column names, not positions)
  for (nm in base_cols) {
    sev_nm   <- paste0(nm, "_severity")
    photo_nm <- paste0(nm, "_photo")
    sev   <- if (sev_nm %in% names(df)) df[[sev_nm]] else ""
    photo <- if (photo_nm %in% names(df)) df[[photo_nm]] else ""
    sev   <- as.character(sev)
    photo <- as.character(photo)
    df[[nm]] <- paste(df[[nm]], sev, photo, sep = ",")
  }
  # Remove severity and photo cols
  df <- df %>% dplyr::select(-dplyr::matches("severity|[0-9]+_photo$"))
  # Elongate
  df_long <- df %>%
    tidyr::pivot_longer(
      cols = dplyr::matches("^(abiotic|disease|pest|weed)[0-9]+$"),
      names_to = "type",
      values_to = "value"
    ) %>%
    tidyr::separate_wider_delim(
      value,
      delim = ",",
      names = c("stressor", "severity", "photo"),
      too_few = "align_start"
    )
  df_long$type <- sub("[0-9]+$", "", df_long$type)
  df_long
}


