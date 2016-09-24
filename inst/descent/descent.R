
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

missing_parent <- function(df, mother, father, missing) {
  df$`One missing parent` <-
    xor(df[[mother]] == missing, df[[father]] == missing)

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

    df <- missing_parent(df, mother, father, missing)

    df <-
      df %>%
      filter(`Missing egos` |
               `Duplicate egos` |
               `Female father` |
               `Male mother` |
               `One missing parent`)


    return(df)

  }

complete_pedigree <-
  function(df,
           ego,
           father,
           mother,
           sex,
           male,
           female,
           missing) {
    # new_rows <- df[FALSE, c(ego, father, mother, sex)]

    # Adds parents to egos if necessary
    # Fathers, mothers not in egos

    if (is.numeric(df[[father]]))
      missing <- as.numeric(missing)

    new_males <- setdiff(df[[father]], c(df[[ego]], missing))
    new_females <- setdiff(df[[mother]], c(df[[ego]], missing))

    if (!(length(new_males) | length(new_females)))
      return(df)

    new_rows <- data_frame(
      ego = c(new_males, new_females),
      father = missing,
      mother = missing,
      sex = c(rep(male, length(new_males)), rep(female, length(new_females)))
    )

    names(new_rows) <- c(ego, father, mother, sex)

    df <- bind_rows(df, new_rows)
    return(df)
  }

as.pedigree.K2 <-
  function(df,
           ego,
           father,
           mother,
           sex,
           male,
           female,
           missing) {
    # takes a data frame and returns a kinship2 pedigree
    #
    # is sex is not in "male", "female", "m", "f", "1", "2", then
    # it is converted to "male", "female"

    e_df <-
      error_df(df, ego, mother, father, sex, female, male, missing)
    if (is.data.frame(e_df) && nrow(e_df) != 0) {
      return(NULL)
    }

    if ((!tolower(male) %in% c('male', 'm', "1")) |
        (!tolower(female) %in% c('female', 'f', "2"))) {
      df[[sex]][df[[sex]] == male] <- 'male'
      df[[sex]][df[[sex]] == female] <- 'female'
    }

    df <-
      complete_pedigree(df, ego, father, mother, sex, male, female, missing)

    ped <-
      kinship2::pedigree(df[[ego]], df[[father]], df[[mother]], df[[sex]], missid = missing)
    return(ped)
  }

as.pedigree.GL <-
  function(df,
           ego,
           father,
           mother,
           sex,
           male,
           female,
           missing) {
    # takes a data frame and returns a GENLIB genealogy (GLgen)
    #
    # ego, mother, father ids are converted to integer values
    # sex is converted to 1 for males and 2 for females
    # missing value is converted to 0
    # parents are added to egos if not already present

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

    return(gen.genealogy(ped, autoComplete = T))

  }

mean_group_relatedness <-
  function(df,
           ego,
           father,
           mother,
           sex,
           male,
           female,
           missing,
           group) {

    ped <- as.pedigree.K2(df,
                          ego,
                          father,
                          mother,
                          sex,
                          male,
                          female,
                          missing)

    kinmat <- kinship(ped) * 2

    submatrix_mean <- function(g){
      egos <- df[[ego]][df[[group]] == g] # egos in group g
      if (length(egos) == 1) return(1)
      m <- kinmat[as.character(egos), as.character(egos)] # submatrix for egos in group g
      mean(m[upper.tri(m)])
    }

    unique_groups <- table(df[[group]])

    group_means <- sapply(names(unique_groups), FUN = submatrix_mean)

    df_group <- data_frame(
      `Group id` = names(unique_groups),
      `Group size` = as.numeric(unique_groups),
      `Mean relatedness` = as.numeric(group_means)
    )

    # names(group_means) <- unique_groups
    return(df_group)

  }
