
library(kinship2)
library(GENLIB)
library(dplyr)

missing_egos <- function(df, ego, missing) {
  if (is.na(missing) | sum(is.na(df[[ego]])) > 0) {
    df$`Missing egos` <- is.na(df[[ego]])
  } else {
    df$`Missing egos` <- df[[ego]] == missing
  }

  return(df)
}

duplicated_egos <- function(df, ego) {
  df$`Duplicate egos` <-
    duplicated(df[ego]) | duplicated(df[ego], fromLast = T)
  return(df)

}

parent_wrong_sex <-
  function(df, ego, mother, father, sex, female, male) {
    ego_fathers <- intersect(df[[father]], df[[ego]])
    df$`Female father` <-
      (df[[sex]] == female) & (df[[ego]] %in% ego_fathers)

    ego_mothers <- intersect(df[[mother]], df[[ego]])
    df$`Male mother` <-
      (df[[sex]] == male) & (df[[ego]] %in% ego_mothers)

    return(df)

  }

error_df <-
  function(df,
           ego,
           mother,
           father,
           sex,
           female,
           male,
           missing) {
    df <- missing_egos(df, ego, missing)
    df <- duplicated_egos(df, ego)
    df <-
      parent_wrong_sex(df, ego, mother, father, sex, female, male)

    df <-
      df %>%
      filter(`Missing egos` |
               `Duplicate egos` | `Female father` | `Male mother`)

    return(df)

  }

as.pedigree <-
  function(df,
           ego,
           mother,
           father,
           sex,
           female,
           male,
           missing) {
    # takes a data frame and returns an equivalent data frame
    # in the correct format for kinship2 and GENLIB
    #
    # ego, mother, father must be integer values
    # sex must be 1 for males and 2 for females
    # missing value must be 0

    # Don't process df if it has errors
    e_df <-
      error_df(df, ego, mother, father, sex, female, male, missing)
    if (is.data.frame(e_df) && nrow(e_df) != 0) {
      return(NULL)
    }

    # First convert all id codes to integers

    ids <- sort(unique(c(df[[ego]], df[[mother]], df[[father]])))

    if (is.numeric(ids)) {
      # Use numeric codes as is

      id_map <- ids
      names(id_map) <- ids

    } else {
      # Convert alphanumeric codes to integers
      id_map <- 1:length(ids)
      names(id_map) <- ids

    }

    # convert missing code to 0
    # and if 0 is an id code, but not the missing code, then
    # convert it to something else

    if (!is.na(id_map[as.character(missing)])) {
      # missing value in ids

      miss <- as.numeric(missing)

      if (is.na(miss) | miss != 0) {
        # alpha missing value or numeric missing value that != 0
        id_map[missing] <- 0
      }
    }

    # Convert males to 1, females to 2
    sex_map <- c(1, 2)
    names(sex_map) <- c(male, female)

    ped <- data.frame(
      ind = unname(id_map[as.character(df[[ego]])]),
      father = unname(id_map[as.character(df[[father]])]),
      mother = unname(id_map[as.character(df[[mother]])]),
      sex = unname(sex_map[as.character(df[[sex]])])
    )

    return(ped)

  }
