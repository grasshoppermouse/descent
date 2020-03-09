

# Cejal gen_params, for testing
gen_params <- list(ego = 'Ego', father = 'Father', mother = 'Mother', sex = 'Sex', missing = '999', male = 'm', female = 'f', livingdead = 'All living', living = '', dead = '')
#
# La Presa gen_params, for testing
# gen_params <- list(ego = 'EID', father = 'FID', mother = 'MID', sex = 'Sex', male = 1, female = 0, missing = 0, livingdead = 'All living', living = '', dead = '')

# Quinlan gen_params
gen_params <-
  list(
    ego = 'Eid#',
    father = 'Fid#',
    mother = 'Mid#',
    sex = 'Sex',
    male = 1,
    female = 0,
    missing = 9999,
    livingdead = 'All living',
    living = '',
    dead = ''
  )

library(kinship2)
library(GENLIB)
library(tidyverse)

# Error checks

check_sex_codes <- function(df, gen_params) {
  df$`Extraneous sex codes` <-
    !(df[[gen_params$sex]] %in% c(gen_params$male, gen_params$female, gen_params$missing))
  return(df)
}

check_livedead_codes <- function(df, gen_params) {
  if (gen_params$livingdead == 'All living')
    df$`Extraneous living-dead codes` <- FALSE
  else
    df$`Extraneous living-dead codes` <-
      !(df[[gen_params$livingdead]] %in% c(gen_params$living, gen_params$dead, gen_params$missing))
  return(df)
}

check_missing_egos <- function(df, gen_params) {
  df$`Missing egos` <-
    is.na(df[[gen_params$ego]]) |
    (df[[gen_params$ego]] == gen_params$missing)
  return(df)
}

check_duplicated_egos <- function(df, gen_params) {
  df$`Duplicate egos` <-
    duplicated(df[gen_params$ego]) |
    duplicated(df[gen_params$ego], fromLast = T)
  return(df)
}

check_ego_equals_parent <- function(df, gen_params) {
  df$`Ego equals parent` <-
    df[[gen_params$ego]] == df[[gen_params$father]] |
    df[[gen_params$ego]] == df[[gen_params$mother]]
  return(df)
}

check_father_equals_mother <- function(df, gen_params) {
  both <- intersect(df[[gen_params$father]], df[[gen_params$mother]])
  both <- setdiff(both, gen_params$missing)
  df$`Father equals mother` <-
    df[[gen_params$father]] %in% both |
    df[[gen_params$mother]] %in% both
  return(df)
}

check_parent_wrong_sex <- function(df, gen_params) {
    ego_fathers <-
      intersect(df[[gen_params$father]], df[[gen_params$ego]])
    df$`Female father` <-
      (df[[gen_params$sex]] == gen_params$female) &
      (df[[gen_params$ego]] %in% ego_fathers)

    ego_mothers <-
      intersect(df[[gen_params$mother]], df[[gen_params$ego]])
    df$`Male mother` <-
      (df[[gen_params$sex]] == gen_params$male) &
      (df[[gen_params$ego]] %in% ego_mothers)

    return(df)
  }

check_missing_parent <- function(df, gen_params) {
  df$`One missing parent` <-
    xor(df[[gen_params$mother]] == gen_params$missing, df[[gen_params$father]] == gen_params$missing)
  return(df)
}

validParams <- function(params, df=NULL) {
  req_params <- c('ego', 'father', 'mother', 'sex', 'male', 'female', 'missing')
  if (sum(map_lgl(params[req_params], ~ !isTruthy(.)) > 0)) {
    return(F)
  }
  if (length(unique(params[req_params[1:5]])) != length(req_params[1:5])) {
    return(F)
  }
  if (!is.null(df) & length(intersect(params[req_params[1:4]], names(df))) < 4){
    return(F)
  }
  return(T)
}

error_df <- function(df, gen_params) {

  if (!validParams(gen_params)) {
    print(gen_params)
    return(
      tibble(Error = 'The same column has been assigned to different variables or values have not been entered. To fix, return to File tab.')
    )
  }

  original_vars <- names(df)

  df <- check_sex_codes(df, gen_params)
  df <- check_livedead_codes(df, gen_params)
  df <- check_missing_egos(df, gen_params)
  df <- check_duplicated_egos(df, gen_params)
  df <- check_ego_equals_parent(df, gen_params)
  df <- check_father_equals_mother(df, gen_params)
  df <- check_parent_wrong_sex(df, gen_params)

  error_vars <- setdiff(names(df), original_vars)

  df <-
    df %>%
    dplyr::filter_at(vars(error_vars), any_vars(. == T))

  if (nrow(df) == 0) {
    return(NULL)
  }

  keep_cols <-
    c(original_vars, error_vars[which(colSums(df[error_vars]) > 0)])

  return(df[keep_cols])
}

# Warning checks

incest <- function(df, gen_params) {
  fm <-
    cbind(df[[gen_params$father]], df[[gen_params$mother]])[df[[gen_params$father]] != gen_params$missing &
                                                              df[[gen_params$mother]] != gen_params$missing, ]
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

  df$`Sibling incest` <-
    apply(cbind(df[[gen_params$father]], df[[gen_params$mother]]), 1, same_mom_or_dad)
  return(df)
}

warning_df <- function(df, gen_params) {
  if (!validParams(gen_params))
    return(NULL)

  original_vars <- names(df)

  df <- incest(df, gen_params)

  warning_vars <- setdiff(names(df), original_vars)

  df <-
    df %>%
    dplyr::filter_at(vars(warning_vars), any_vars(. == T))

  if (nrow(df) == 0)
    return(NULL)

  keep_cols <-
    c(original_vars, warning_vars[which(colSums(df[warning_vars]) > 0)])

  return(df[keep_cols])

}

summary_stats <- function(df, gen_params) {
  # df <- complete_pedigree(df, gen_params)

  df2 = as.ped(df, gen_params)
  ped = gen.genealogy(df2, autoComplete = T)

  kd <- tibble(
    id = df2$ind,
    sex = df2$sex,
    depth = gen.meangendepth(ped, pro = df2$ind, type = 'IND')[[1]]
  )

  stats <- list(Note = "Parents who were not also egos have been added as egos with missing parents\n")
  stats$`Number of egos` <- gen.noind(ped)
  stats$`Number of males` <- gen.nomen(ped)
  stats$`Number of females` <- gen.nowomen(ped)
  stats$`Number of founders` <- length(gen.founder(ped))
  stats$`Number of generations` <- gen.depth(ped)

  if ('matrilineage' %in% names(df)){
    stats$`Number of matrilineages` <- length(unique(df$matrilineage))
  }
  if ('patrilineage' %in% names(df)){
    stats$`Number of patrilineages` <- length(unique(df$patrilineage))
  }


  return(list(stats = stats, kd = kd))
}

complete_pedigree <- function(df, gen_params) {
  # new_rows <- df[FALSE, c(ego, father, mother, sex)]

  # Adds parents to egos if necessary
  # Fathers, mothers not in egos

  if (! validParams(gen_params, df)) {
    warning('params not valid; returning original df')
    return(df)
  }

  if (is.numeric(df[[gen_params$father]])) {
    miss <- as.numeric(gen_params$missing)
  } else {
    miss <- gen_params$missing
  }

  new_males <-
    setdiff(df[[gen_params$father]], c(df[[gen_params$ego]], miss))
  new_females <-
    setdiff(df[[gen_params$mother]], c(df[[gen_params$ego]], miss))

  if (!(length(new_males) | length(new_females))) {
    return(df)
  }

  new_rows <- tibble(
    ego = c(new_males, new_females),
    father = miss,
    mother = miss,
    sex = c(
      rep(gen_params$male, length(new_males)),
      rep(gen_params$female, length(new_females))
    )
  )

  names(new_rows) <-
    c(gen_params$ego,
      gen_params$father,
      gen_params$mother,
      gen_params$sex)

  # print(gen_params)
  # print(str(df, give.attr=F))
  # print(str(new_rows, give.attr=F))
  df <- bind_rows(df, new_rows)
  return(df)
}

create_id_map <- function(df, gen_params) {
  # Convert all id codes to integers
  # Create dicts to convert original ids to new ids,
  # and new ids to original ids

  ids <-
    sort(unique(c(df[[gen_params$ego]], df[[gen_params$mother]], df[[gen_params$father]])))

  if (sum(is.na(as.numeric(ids))) == 0) {
    # Use numeric codes as is

    id_map <- as.numeric(ids)
    names(id_map) <- ids

    # convert missing code to 0
    #
    # If 0 is an id code, but not the missing code, then
    # convert it to something else

    if (as.numeric(gen_params$missing) != 0 & 0 %in% id_map) {
      newid <- max(ids) + 1
      ids_map[0] <- newid
    }
    id_map[id_map == as.numeric(gen_params$missing)] <- 0

  } else {
    # Convert alphanumeric codes to integers
    id_map <- 1:length(ids)
    names(id_map) <- ids
    id_map[gen_params$missing] <- 0
  }

  rev_id_map <- names(id_map)
  names(rev_id_map) <- unname(id_map)

  return(list(id_map = id_map, rev_id_map = rev_id_map))
}

as.ped <- function(df, gen_params, complete = F, include_original = F) {
    # takes a data frame and returns a data frame in
    # format required for a GENLIB genealogy (GLgen)
    #
    # ego, mother, father ids are converted to integer values
    # sex is converted to 1 for males and 2 for females
    # missing value is converted to 0
    # parents are added to egos if not already present

    # Don't process df if it has errors (other than one missing parent?)
    # e_df <-
    #   error_df(df, gen_params)
    # if (is.data.frame(e_df) && nrow(e_df) > 0) {
    #   return(NULL)
    # }

    if (complete){
      df <- complete_pedigree(df, gen_params)
      }
    maps <- create_id_map(df, gen_params)
    id_map <- maps$id_map

    # Convert males to 1, females to 2
    sex_map <- c(1, 2)
    names(sex_map) <- c(gen_params$male, gen_params$female)

    ped <-
      data.frame(
        ind = unname(id_map[as.character(df[[gen_params$ego]])]),
        father = unname(id_map[as.character(df[[gen_params$father]])]),
        mother = unname(id_map[as.character(df[[gen_params$mother]])]),
        sex = unname(sex_map[as.character(df[[gen_params$sex]])]),
        stringsAsFactors = F
      )

    if (include_original) {
      ped$original_ind <- map$rev_id_map[ped$id]
      ped <-
        ped[c('original_ind', 'ind', 'father', 'mother', 'sex')]
    }

    return(ped)

  }

write_ped <- function(df,
           gen_params,
           path,
           complete = T,
           include_original = T) {
    # Converts df to GENLIB format and writes a tsv file
    readr::write_tsv(x = as.ped(df, gen_params, complete, include_original),
                     path = path)
  }

as.genealogy.GL <- function(df, gen_params) {
  gen.genealogy(as.ped(df, gen_params), autoComplete = T)
}

as.genealogy.K2 <- function(df, gen_params) {
    # takes a data frame and returns a kinship2 pedigree
    #
    # is sex is not in "male", "female", "m", "f", "1", "2", then
    # it is converted to "male", "female"

    df_err <-
      error_df(df, gen_params)

    if (!is.null(df_err)) {
      return(NULL)
    }

    if ((!tolower(gen_params$male) %in% c('male', 'm', "1")) |
        (!tolower(gen_params$female) %in% c('female', 'f', "2"))) {
      df[[gen_params$sex]][df[[gen_params$sex]] == gen_params$male] <-
        'male'
      df[[gen_params$sex]][df[[gen_params$sex]] == gen_params$female] <-
        'female'
    }

    df <-
      complete_pedigree(df, gen_params)

    famid <-
      makefamid(df[[gen_params$ego]], df[[gen_params$father]], df[[gen_params$mother]])

    ped <-
      kinship2::pedigree(df[[gen_params$ego]],
                         df[[gen_params$father]],
                         df[[gen_params$mother]],
                         df[[gen_params$sex]],
                         famid = famid,
                         missid = gen_params$missing)
    return(ped)
  }

mean_group_kinship <- function(kinmat, df, gen_params, group_var, nosingles=F) {
    # if (nrow(df) == 0) return(NULL)
    # gen <- as.genealogy.GL(df, gen_params)
    #
    # kinmat <- genphi2(gen, df, gen_params)

    submatrix_mean <- function(g) {
      egos <- df[[gen_params$ego]][df[[group_var]] == g] # egos in group g
      m <-
        kinmat[as.character(egos), as.character(egos)] # submatrix for egos in group g
      if (length(egos) == 1) {
        return(m)
      }
      mean(m[upper.tri(m)])
    }

    unique_groups <- table(df[[group_var]])
      # table(df[[group_var]][df[[group_var]] != gen_params$missing])

    group_means <- sapply(names(unique_groups), FUN = submatrix_mean)

    df_group <- tibble(
      `Group id` = names(unique_groups),
      `Group size` = as.numeric(unique_groups),
      `Mean kinship` = as.numeric(group_means)
    )

    if(nosingles){
      df_group <- dplyr::filter(df_group, `Group size` > 1)
    }

    # names(group_means) <- unique_groups
    return(df_group)

  }

exclude_founders <- function(df, gen_params) {
  eid <- df[[gen_params$ego]]
  fid <- df[[gen_params$father]]
  mid <- df[[gen_params$mother]]
  missing <- gen_params$missing
  return(eid[fid != missing & mid != missing])
}

# wraps gen.phi to convert ids, if necessary
genphi2 <- function(gen, df, gen_params) {
  maps <- create_id_map(df, gen_params)
  id_map <- maps$id_map
  rev_id_map <- maps$rev_id_map

  mat <- gen.phi(gen, pro = id_map[as.character(df[[gen_params$ego]])])
  rownames(mat) <- rev_id_map[(rownames(mat))]
  colnames(mat) <- rev_id_map[(colnames(mat))]

  return(mat)
}

# wraps gen.f to convert ids, if necessary
genf2 <- function(gen, df, gen_params) {
  maps <- create_id_map(df, gen_params)
  id_map <- maps$id_map
  rev_id_map <- maps$rev_id_map

  vf <- gen.f(gen, pro = id_map[as.character(df[[gen_params$ego]])])
  names(vf) <- rev_id_map[names(vf)]
  return(vf)
}

# wraps gen.f to convert ids, if necessary
gengraph2 <- function(gen, df, gen_params, ego, ancestors=NULL) {
  if (length(ego) == 0) return(NULL)
  maps <- create_id_map(df, gen_params)
  id_map <- maps$id_map
  rev_id_map <- maps$rev_id_map

  if (is.null(ancestors)){
    gen.graph(gen, pro = id_map[as.character(ego)])
  } else {
    gen.graph(gen, pro = id_map[as.character(ego)], ancestors = ancestors)
  }

}

relatedness <- function(mat) {
  # Bulmer, M. (1994). Theoretical Evolutionary Ecology. Sunderland, Massachusetts: Sinauer.
  # Rxy =Fxy/Fxx.
  mat / diag(mat)
}

oldlineage <- function(ped, maternal){
  # maternal = T, paternal = F
  # This is pretty slow
  map_int(ped$ind, ~ gen.founder(gen.lineages(ped, pro = ., maternal = maternal)))
}

# matrilineage <- function(ped){
#   ped$matrilineage <- NA
#   for (i in 1:nrow(ped)){
#     if (is.na(ped$matrilineage[i])){
#       matrilineage <- ped$ind[i]
#       mother <- ped$mother[i]
#       if (mother == 0){
#         ped$matrilineage[i] <- matrilineage
#         next
#       }
#       while (mother != 0){
#         matrilineage <- union(matrilineage, ped$ind[ped$mother == mother])
#         lastmother <- mother
#         mother <- ped$mother[ped$ind == mother]
#       }
#       ped$matrilineage[ped$ind %in% matrilineage] <- lastmother
#     }
#   }
#   return(ped$matrilineage)
# }

lineage <- function(df, gen_params, type){

  # Return NULL, or something else?
  if (is.null(df)) return(NULL)
  if (! validParams(gen_params)) return(NULL)

  if (type == 'matrilineage'){
    parent <- gen_params$mother
  } else if (type == 'patrilineage'){
    parent <- gen_params$father
  } else {
    stop("type must be either 'matrilineage' or 'patrilineage'")
  }

  egos <- df[[gen_params$ego]]
  parents <- df[[parent]]
  missing <- gen_params$missing

  lineage_id <- rep(NA, length(egos))

  for (i in 1:length(egos)){
    if (is.na(lineage_id[i])){
      p <- parents[i]
      if (p == missing){
        lineage_id[i] <- egos[i]
        next
      }
      lineage <- egos[i]
      while (p != missing){
        lineage <- union(lineage, egos[parents == parent])
        lastparent <- p
        p <- parents[egos == p]
      }
      lineage_id[egos %in% lineage] <- lastparent
    }
  }
  return(lineage_id)
}

lineage_ids <- function(v, omit_singles=T){
  if (omit_singles){
    v2 <- table(v)
    v2 <- v2[v2>1]
    v <- names(v2)
  } else {
    v <- unique(v)
  }
  isnumeric <- sum(is.na(as.numeric(v))) == 0
  if (isnumeric){
    v <- as.numeric(v)
  }
  return(sort(v))
}

lineage_members <- function(df, gen_params, lineage_id, type, omit_founders=T, add_other_parent=F){
  ego_var <- gen_params$ego
  pros <- df[[ego_var]][df[[type]] == lineage_id] # members of lineage
  if (omit_founders){
    nonfounders <- exclude_founders(df, gen_params) # vector of egos with at least 1 parent
    pros <- intersect(pros, nonfounders)
  }
  if (add_other_parent){
    if (type == 'matrilineage') parent_var <- gen_params$father
    if (type == 'patrilineage') parent_var <- gen_params$mother
    for (pro in pros){
      pros <- union(pros, df[[parent_var]][df[[ego_var]]==pro])
    }
  }
  return(pros)
}
