
library(kinship2)
library(GENLIB)
library(dplyr)

# Error checks

check_sex_codes <- function(df, gen_params){
  df$`Extraneous sex codes` <- ! (df[[gen_params$sex]] %in% c(gen_params$male, gen_params$female, gen_params$missing))
  return(df)
}

check_livedead_codes <- function(df, gen_params){
  if(gen_params$livingdead == 'All living')
    df$`Extraneous living-dead codes` <- FALSE
  else
    df$`Extraneous living-dead codes` <- ! (df[[gen_params$livingdead]] %in% c(gen_params$living, gen_params$dead, gen_params$missing))
  return(df)
}

missing_egos <- function(df, gen_params) {
  if (is.na(gen_params$missing) | sum(is.na(df[[gen_params$ego]])) > 0) {
    df$`Missing egos` <- is.na(df[[gen_params$ego]])
  } else {
    df$`Missing egos` <- df[[gen_params$ego]] == gen_params$missing
  }

  return(df)
}

duplicated_egos <- function(df, gen_params) {
  df$`Duplicate egos` <-
    duplicated(df[gen_params$ego]) | duplicated(df[gen_params$ego], fromLast = T)

  return(df)
}

ego_equals_parent <- function(df, gen_params){
  df$`Ego equals parent` <- df[[gen_params$ego]] == df[[gen_params$father]] | df[[gen_params$ego]] == df[[gen_params$mother]]
  return(df)
}

father_equals_mother <- function(df, gen_params){
  both <- intersect(df[[gen_params$father]], df[[gen_params$mother]])
  both <- setdiff(both, gen_params$missing)
  df$`Father equals mother` <- df[[gen_params$father]] %in% both | df[[gen_params$mother]] %in% both
  return(df)
}

parent_wrong_sex <-
  function(df, gen_params) {
    ego_fathers <- intersect(df[[gen_params$father]], df[[gen_params$ego]])
    df$`Female father` <-
      (df[[gen_params$sex]] == gen_params$female) & (df[[gen_params$ego]] %in% ego_fathers)

    ego_mothers <- intersect(df[[gen_params$mother]], df[[gen_params$ego]])
    df$`Male mother` <-
      (df[[gen_params$sex]] == gen_params$male) & (df[[gen_params$ego]] %in% ego_mothers)

    return(df)
  }

missing_parent <- function(df, gen_params) {
  df$`One missing parent` <-
    xor(df[[gen_params$mother]] == gen_params$missing, df[[gen_params$father]] == gen_params$missing)

  return(df)
}

error_df <-
  function(df, gen_params) {

    df <- check_sex_codes(df, gen_params)
    df <- missing_egos(df, gen_params)
    df <- duplicated_egos(df, gen_params)
    df <- ego_equals_parent(df, gen_params)
    df <- father_equals_mother(df, gen_params)
    df <-
      parent_wrong_sex(df, gen_params)
    df <- missing_parent(df, gen_params)
    df <- check_livedead_codes(df, gen_params)

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
  function(df, gen_params) {

    fm <-
      cbind(df[[gen_params$father]], df[[gen_params$mother]])[df[[gen_params$father]] != gen_params$missing &
                                          df[[gen_params$mother]] != gen_params$missing,]
    fathers_mothers <- unique(t(split(fm, seq_len(nrow(
      fm
    )))))

    df$`Mother-son incest` <- FALSE

    sons <- df[gen_params$sex] == gen_params$male
    sons_mothers <-
      split(cbind(df[[gen_params$ego]][sons], df[[gen_params$mother]][sons]), seq_len(sum(sons)))
    mother_son_incest <- intersect(sons_mothers, fathers_mothers)

    if (length(mother_son_incest) != 0) {
      df$`Mother-son incest` <- sapply(mother_son_incest,
                                       function(x)
                                         df[[gen_params$mother]] == x[2] &
                                         (df[[gen_params$ego]] == x[1] |
                                            df[[gen_params$father]] == x[1]))
    }

    df$`Father-daughter incest` <- FALSE

    daughters <- df[gen_params$sex] == gen_params$female
    fathers_daughters <-
      split(cbind(df[[gen_params$father]][daughters], df[[gen_params$ego]][daughters]), seq_len(sum(daughters)))
    father_daughter_incest <-
      intersect(fathers_daughters, fathers_mothers)

    if (length(father_daughter_incest) != 0) {
      df$`Father-daughter incest` <- sapply(father_daughter_incest,
                                            function(x)
                                              df[[gen_params$father]] == x[1] &
                                              (df[[gen_params$ego]] == x[2] |
                                                 df[[gen_params$mother]] == x[2]))
    }

    df$`Sibling incest` <- FALSE

    # Named vectors for quick lookup of ego's mom or dad
    mom <- df[[gen_params$mother]]
    names(mom) <- df[[gen_params$ego]]

    dad <- df[[gen_params$father]]
    names(dad) <- df[[gen_params$ego]]

    same_mom_or_dad <- function(mates) {
      mom1 <- mom[as.character(mates[1])]
      mom2 <- mom[as.character(mates[2])]
      if (!is.na(mom1) & !is.na(mom2) &
          mom1 != gen_params$missing &
          mom2 != gen_params$missing & mom1 == mom2)
        return(TRUE)

      dad1 <- dad[as.character(mates[1])]
      dad2 <- dad[as.character(mates[2])]
      if (!is.na(dad1) & !is.na(dad2) &
          dad1 != gen_params$missing &
          dad2 != gen_params$missing & dad1 == dad2)
        return(TRUE)

      return(FALSE)
    }

    df$`Sibling incest` <- apply(cbind(df[[gen_params$father]], df[[gen_params$mother]]), 1, same_mom_or_dad)
    return(df)
  }

warning_df <-
  function(df, gen_params) {
    df <- incest(df, gen_params)
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

summary_stats <- function(df, gen_params) {

  df <- complete_pedigree(df, gen_params)

  ped <- as.pedigree.K2(df, gen_params)

  kd <- data_frame(
    id = ped$id,
    depth = kindepth(ped)
  )

  stats <- list(Note = "Parents who are not also egos have been added as egos with missing parents\n")
  stats$`Number of egos` <- nrow(df)
  stats$`Number of males` <- sum(df[[gen_params$sex]] == gen_params$male)
  stats$`Number of females` <- sum(df[[gen_params$sex]] == gen_params$female)
  stats$`Number of founders` <- sum(kd$depth == 0)
  stats$`Number of generations` <- max(kd$depth) + 1


  return(
    list(
      stats = stats,
      kd = kd
    )
  )
}

complete_pedigree <-
  function(df, gen_params) {
    # new_rows <- df[FALSE, c(ego, father, mother, sex)]

    # Adds parents to egos if necessary
    # Fathers, mothers not in egos

    if (is.numeric(df[[gen_params$father]]))
      miss <- as.numeric(gen_params$missing)

    new_males <- setdiff(df[[gen_params$father]], c(df[[gen_params$ego]], miss))
    new_females <- setdiff(df[[gen_params$mother]], c(df[[gen_params$ego]], miss))

    if (!(length(new_males) | length(new_females)))
      return(df)

    new_rows <- data_frame(
      ego = c(new_males, new_females),
      father = miss,
      mother = miss,
      sex = c(rep(gen_params$male, length(new_males)), rep(gen_params$female, length(new_females)))
    )

    names(new_rows) <- c(gen_params$ego, gen_params$father, gen_params$mother, gen_params$sex)

    df <- bind_rows(df, new_rows)
    return(df)
  }

as.pedigree.K2 <-
  function(df, gen_params) {

    # takes a data frame and returns a kinship2 pedigree
    #
    # is sex is not in "male", "female", "m", "f", "1", "2", then
    # it is converted to "male", "female"

    df_err <-
      error_df(df, gen_params)

    if (! is.null(df_err)) {
      return(NULL)
    }

    if ((!tolower(gen_params$male) %in% c('male', 'm', "1")) |
        (!tolower(gen_params$female) %in% c('female', 'f', "2"))) {
      df[[gen_params$sex]][df[[gen_params$sex]] == gen_params$male] <- 'male'
      df[[gen_params$sex]][df[[gen_params$sex]] == gen_params$female] <- 'female'
    }

    df <-
      complete_pedigree(df, gen_params)

    ped <-
      kinship2::pedigree(df[[gen_params$ego]], df[[gen_params$father]], df[[gen_params$mother]], df[[gen_params$sex]], missid = gen_params$missing)
    return(ped)
  }

as.pedigree.GL <-
  function(df, gen_params) {

    # takes a data frame and returns a GENLIB genealogy (GLgen)
    #
    # ego, mother, father ids are converted to integer values
    # sex is converted to 1 for males and 2 for females
    # missing value is converted to 0
    # parents are added to egos if not already present

    # Don't process df if it has errors
    e_df <-
      error_df(df, gen_params)
    if (is.data.frame(e_df) && nrow(e_df) != 0) {
      return(NULL)
    }

    # First convert all id codes to integers

    ids <- sort(unique(c(df[[gen_params$ego]], df[[gen_params$mother]], df[[gen_params$father]])))

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

    if (!is.na(id_map[as.character(gen_params$missing)])) {
      # missing value in ids

      miss <- as.numeric(gen_params$missing)

      if (is.na(miss) | miss != 0) {
        # alpha missing value or numeric missing value that != 0
        id_map[gen_params$missing] <- 0
      }
    }

    # Convert males to 1, females to 2
    sex_map <- c(1, 2)
    names(sex_map) <- c(gen_params$male, gen_params$female)

    ped <- data.frame(
      ind = unname(id_map[as.character(df[[gen_params$ego]])]),
      father = unname(id_map[as.character(df[[gen_params$father]])]),
      mother = unname(id_map[as.character(df[[gen_params$mother]])]),
      sex = unname(sex_map[as.character(df[[gen_params$sex]])])
    )

    return(gen.genealogy(ped, autoComplete = T))

  }

mean_group_relatedness <-
  function(df, gen_params, group) {

    ped <- as.pedigree.K2(df, gen_params)

    if (is.null(ped)) return(NULL)

    kinmat <- kinship(ped) * 2

    submatrix_mean <- function(g) {
      egos <- df[[gen_params$ego]][df[[group]] == g] # egos in group g
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
