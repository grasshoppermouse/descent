
library(kinship2)
library(GENLIB)
library(dplyr)

# Error checks

check_sex_codes <- function(df, sex, male, female, missing){
  df$`Extraneous sex codes` <- ! (df[[sex]] %in% c(male, female, missing))
  return(df)
}

check_livedead_codes <- function(df, livingdead, living, dead, missing){
  if(livingdead == 'All living')
    df$`Extraneous living-dead codes` <- FALSE
  else
    df$`Extraneous living-dead codes` <- ! (df[[livingdead]] %in% c(living, dead, missing))
  return(df)
}

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

ego_equals_parent <- function(df, ego, father, mother){
  df$`Ego equals parent` <- df[[ego]] == df[[father]] | df[[ego]] == df[[mother]]
  return(df)
}

father_equals_mother <- function(df, father, mother, missing){
  both <- intersect(df[[father]], df[[mother]])
  both <- setdiff(both, missing)
  df$`Father equals mother` <- df[[father]] %in% both | df[[mother]] %in% both
  return(df)
}

parent_wrong_sex <-
  function(df, ego, father, mother, sex, male, female) {
    ego_fathers <- intersect(df[[father]], df[[ego]])
    df$`Female father` <-
      (df[[sex]] == female) & (df[[ego]] %in% ego_fathers)

    ego_mothers <- intersect(df[[mother]], df[[ego]])
    df$`Male mother` <-
      (df[[sex]] == male) & (df[[ego]] %in% ego_mothers)

    return(df)
  }

missing_parent <- function(df, father, mother, missing) {
  df$`One missing parent` <-
    xor(df[[mother]] == missing, df[[father]] == missing)

  return(df)
}

error_df <-
  function(df,
           ego,
           father,
           mother,
           sex,
           male,
           female,
           livingdead,
           living,
           dead,
           missing) {

    df <- check_sex_codes(df, sex, male, female, missing)
    df <- missing_egos(df, ego, missing)
    df <- duplicated_egos(df, ego)
    df <- ego_equals_parent(df, ego, father, mother)
    df <- father_equals_mother(df, father, mother, missing)
    df <-
      parent_wrong_sex(df, ego, father, mother, sex, male, female)
    df <- missing_parent(df, father, mother, missing)
    df <- check_livedead_codes(df, livingdead, living, dead, missing)

    df <-
      df %>%
      filter(
        `Extraneous sex codes` |
        `Extraneous living-dead codes` |
        `Missing egos` |
        `Duplicate egos` |
        `Ego equals parent` |
        `Father equals mother` |
        `Female father` |
        `Male mother` |
        `One missing parent`)

    if (nrow(df) == 0){
      return(NULL)
    }

    error_cols <- c(
      'Extraneous sex codes',
      'Extraneous living-dead codes',
      'Missing egos',
      'Duplicate egos',
      'Ego equals parent',
      'Father equals mother',
      'Female father',
      'Male mother',
      'One missing parent'
      )

    other_cols <- setdiff(names(df), error_cols)
    keep_cols <-
      c(other_cols, error_cols[which(colSums(df[error_cols]) > 0)])

    return(df[keep_cols])
  }

# Warning checks

incest <-
  function(df,
           ego,
           father,
           mother,
           sex,
           male,
           female,
           missing) {
    fm <-
      cbind(df[[father]], df[[mother]])[df[[father]] != missing &
                                          df[[mother]] != missing,]
    fathers_mothers <- unique(t(split(fm, seq_len(nrow(
      fm
    )))))

    df$`Mother-son incest` <- FALSE

    sons <- df[sex] == male
    sons_mothers <-
      split(cbind(df[[ego]][sons], df[[mother]][sons]), seq_len(sum(sons)))
    mother_son_incest <- intersect(sons_mothers, fathers_mothers)

    if (length(mother_son_incest) != 0) {
      df$`Mother-son incest` <- sapply(mother_son_incest,
                                       function(x)
                                         df[[mother]] == x[2] &
                                         (df[[ego]] == x[1] |
                                            df[[father]] == x[1]))
    }

    df$`Father-daughter incest` <- FALSE

    daughters <- df[sex] == female
    fathers_daughters <-
      split(cbind(df[[father]][daughters], df[[ego]][daughters]), seq_len(sum(daughters)))
    father_daughter_incest <-
      intersect(fathers_daughters, fathers_mothers)

    if (length(father_daughter_incest) != 0) {
      df$`Father-daughter incest` <- sapply(father_daughter_incest,
                                            function(x)
                                              df[[father]] == x[1] &
                                              (df[[ego]] == x[2] |
                                                 df[[mother]] == x[2]))
    }

    df$`Sibling incest` <- FALSE

    # Named vectors for quick lookup of ego's mom or dad
    mom <- df[[mother]]
    names(mom) <- df[[ego]]

    dad <- df[[father]]
    names(dad) <- df[[ego]]

    same_mom_or_dad <- function(mates) {
      mom1 <- mom[as.character(mates[1])]
      mom2 <- mom[as.character(mates[2])]
      if (!is.na(mom1) & !is.na(mom2) &
          mom1 != missing &
          mom2 != missing & mom1 == mom2)
        return(TRUE)

      dad1 <- dad[as.character(mates[1])]
      dad2 <- dad[as.character(mates[2])]
      if (!is.na(dad1) & !is.na(dad2) &
          dad1 != missing &
          dad2 != missing & dad1 == dad2)
        return(TRUE)

      return(FALSE)
    }

    df$`Sibling incest` <- apply(cbind(df[[father]], df[[mother]]), 1, same_mom_or_dad)
    return(df)
  }

warning_df <-
  function(df,
           ego,
           father,
           mother,
           sex,
           male,
           female,
           missing) {
    df <- incest(df, ego, father, mother, sex, male, female, missing)
    df <-
      df %>%
      dplyr::filter((`Mother-son incest` |
                       `Father-daughter incest` |
                       `Sibling incest`))

    if (nrow(df) == 0)
      return(NULL)

    warning_cols <- c('Mother-son incest',
                      'Father-daughter incest',
                      'Sibling incest')

    other_cols <- setdiff(names(df), warning_cols)
    keep_cols <-
      c(other_cols, warning_cols[which(colSums(df[warning_cols]) > 0)])

    return(df[keep_cols])

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

    df_err <-
      error_df(df, ego, mother, father, sex, female, male, missing)

    if (! is.null(df_err)) {
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

    if (is.null(ped)) return(NULL)

    kinmat <- kinship(ped) * 2

    submatrix_mean <- function(g) {
      egos <- df[[ego]][df[[group]] == g] # egos in group g
      if (length(egos) == 1)
        return(1)
      m <-
        kinmat[as.character(egos), as.character(egos)] # submatrix for egos in group g
      mean(m[upper.tri(m)])
    }

    unique_groups <- table(df[[group]])

    group_means <-
      sapply(names(unique_groups), FUN = submatrix_mean)

    df_group <- data_frame(
      `Group id` = names(unique_groups),
      `Group size` = as.numeric(unique_groups),
      `Mean relatedness` = as.numeric(group_means)
    )

    # names(group_means) <- unique_groups
    return(df_group)

  }
