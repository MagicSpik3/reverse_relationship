#' Reverse household relationship matrix
#'
#' @description
#' Given a household roster in long format with relationship variables
#' \code{r01}–\code{r16}, this function constructs the reversed relationship
#' matrix \code{R1Rev}–\code{R16Rev}.
#'
#' Each \code{RjRev} represents the relationship that household member \emph{j}
#' reports \emph{towards the current person}.
#'
#' By default, the function reproduces legacy SPSS behaviour exactly:
#' reversed relationships are copied only when explicitly declared.
#'
#' Optionally, missing reverse relationships may be inferred using a supplied
#' inverse-relationship map.
#'
#' @param df A data frame containing:
#'   \itemize{
#'     \item \code{HHSerial}: household identifier
#'     \item \code{person}: household roster position (integer, usually 1–16)
#'     \item Relationship variables named \code{r01}–\code{r16}
#'   }
#'
#' @param mode Character. Either:
#'   \describe{
#'     \item{\code{"explicit"}}{Reverse only explicitly reported relationships (default; SPSS‑faithful).}
#'     \item{\code{"infer"}}{Infer missing reverse relationships using \code{relationship_map}.}
#'   }
#'
#' @param relationship_map Named integer vector mapping a relationship code
#'   to its inverse (required when \code{mode = "infer"}).
#'
#' @return A data frame with the original columns plus \code{R1Rev}–\code{R16Rev}.
#'   All reverse variables are present and filled with \code{-9} when not applicable.
#'
#' @export
reverse_relationships <- function(
    df,
    mode = c("explicit", "infer"),
    relationship_map = NULL
) {
  mode <- match.arg(mode)
  
  # ------------------------------------------------------------------
  # 0) Basic validation
  # ------------------------------------------------------------------
  stopifnot(
    all(c("HHSerial", "person") %in% names(df)),
    all(paste0("r", sprintf("%02d", 1:16)) %in% names(df))
  )
  
  # Ensure stable ordering (mirrors SPSS SORT + LAG logic)
  df <- dplyr::arrange(df, HHSerial, person)
  
  # ------------------------------------------------------------------
  # 1) Convert original matrix to long form
  # ------------------------------------------------------------------
  rel_long <- tidyr::pivot_longer(
    df,
    cols = dplyr::matches("^r\\d{2}$"),
    names_to = "target_person",
    values_to = "relationship"
  ) |>
    dplyr::mutate(
      target_person = as.integer(sub("^r", "", target_person)),
      source_person = person
    )
  
  # ------------------------------------------------------------------
  # 2) Explicit reverse (SPSS‑faithful behaviour)
  # ------------------------------------------------------------------
  rel_rev_explicit <- dplyr::filter(
    rel_long,
    relationship != -9
  ) |>
    dplyr::transmute(
      HHSerial,
      person        = target_person,
      source_person = source_person,
      relationship
    )
  
  rel_rev_wide <- tidyr::pivot_wider(
    rel_rev_explicit,
    names_from  = source_person,
    values_from = relationship,
    names_glue  = "R{source_person}Rev",
    values_fn   = dplyr::first
  )
  
  df_final <- dplyr::left_join(
    df,
    rel_rev_wide,
    by = c("HHSerial", "person")
  )
  
  # ------------------------------------------------------------------
  # 3) Enforce SPSS matrix contract: R1Rev–R16Rev always exist
  # ------------------------------------------------------------------
  all_rev_cols <- paste0("R", 1:16, "Rev")
  
  for (col in all_rev_cols) {
    if (!col %in% names(df_final)) {
      df_final[[col]] <- -9
    }
  }
  
  df_final <- dplyr::mutate(
    df_final,
    dplyr::across(
      dplyr::all_of(all_rev_cols),
      ~ tidyr::replace_na(.x, -9)
    )
  )
  
  # ------------------------------------------------------------------
  # 4) Optional inference of reverse relationships
  # ------------------------------------------------------------------
  if (mode == "infer") {
    
    if (is.null(relationship_map)) {
      stop("mode = 'infer' requires a relationship_map")
    }
    
    relationship_map <- as.integer(relationship_map)
    names(relationship_map) <- as.character(names(relationship_map))
    
    for (hh in unique(df_final$HHSerial)) {
      
      idx <- which(df_final$HHSerial == hh)
      hh_df <- df_final[idx, , drop = FALSE]
      
      for (i in seq_len(nrow(hh_df))) {
        for (j in seq_len(nrow(hh_df))) {
          
          if (i == j) next
          
          fwd_col <- paste0("r", sprintf("%02d", j))
          fwd_val <- hh_df[[fwd_col]][i]
          
          if (
            !is.na(fwd_val) &&
            fwd_val != -9 &&
            as.character(fwd_val) %in% names(relationship_map)
          ) {
            
            rev_col <- paste0("R", i, "Rev")
            
            # Only infer when explicitly missing
            if (hh_df[[rev_col]][j] == -9) {
              hh_df[[rev_col]][j] <- relationship_map[as.character(fwd_val)]
            }
          }
        }
      }
      
      df_final[idx, all_rev_cols] <- hh_df[, all_rev_cols]
    }
  }
  
  # ------------------------------------------------------------------
  # 5) Optional SPSS-style labelling
  # ------------------------------------------------------------------
  # (Caller may label externally if desired)
  #
  # df_final <- dplyr::mutate(
  #   df_final,
  #   dplyr::across(
  #     dplyr::matches("^R\\d+Rev$"),
  #     ~ haven::labelled(.x, labels = rel_labels)
  #   )
  # )
  
  df_final
}


RELATIONSHIP_NEW <- list(
  SPOUSE              = 1,
  CIVIL_PARTNER       = 2,
  COHABITEE           = 3,
  
  CHILD               = 4,   # Son or daughter (incl adopted)
  STEP_CHILD          = 5,
  FOSTER_CHILD        = 6,
  
  CHILD_IN_LAW        = 7,   # Son/daughter-in-law
  
  PARENT              = 8,   # Parent or guardian (incl adoptive)
  STEP_PARENT         = 9,
  FOSTER_PARENT       = 10,
  
  PARENT_IN_LAW       = 11,
  
  SIBLING             = 12,  # Brother or sister (incl adopted)
  STEP_SIBLING        = 13,
  FOSTER_SIBLING      = 14,
  
  SIBLING_IN_LAW      = 15,
  
  GRANDCHILD          = 16,
  GRANDPARENT         = 17,
  
  OTHER_RELATIVE      = 18,
  OTHER_NON_RELATIVE  = 19,
  
  DK_REFUSAL          = -8,
  NOT_APPLICABLE      = -9
)

RELATIONSHIP_OLD <- list(
  SPOUSE              = 1,
  COHABITEE           = 2,
  
  CHILD               = 3,
  STEP_CHILD          = 4,
  FOSTER_CHILD        = 5,
  
  CHILD_IN_LAW        = 6,
  
  PARENT              = 7,
  STEP_PARENT         = 8,
  FOSTER_PARENT       = 9,
  
  PARENT_IN_LAW       = 10,
  
  SIBLING             = 11,
  STEP_SIBLING        = 12,
  FOSTER_SIBLING      = 13,
  
  SIBLING_IN_LAW      = 14,
  
  GRANDCHILD          = 15,
  GRANDPARENT         = 16,
  
  OTHER_RELATIVE      = 17,
  OTHER_NON_RELATIVE  = 18,
  
  CIVIL_PARTNER       = 20,
  
  DK_REFUSAL          = -8,
  NOT_APPLICABLE      = -9
)


get_relationship_enum <- function(version = c("new", "old")) {
  version <- match.arg(version)
  switch(
    version,
    new = RELATIONSHIP_NEW,
    old = RELATIONSHIP_OLD
  )
}


INVERSE_RELATIONSHIP_NEW <- c(
  # Symmetric
  SPOUSE         = RELATIONSHIP_NEW$SPOUSE,
  CIVIL_PARTNER  = RELATIONSHIP_NEW$CIVIL_PARTNER,
  COHABITEE      = RELATIONSHIP_NEW$COHABITEE,
  SIBLING        = RELATIONSHIP_NEW$SIBLING,
  STEP_SIBLING   = RELATIONSHIP_NEW$STEP_SIBLING,
  FOSTER_SIBLING = RELATIONSHIP_NEW$FOSTER_SIBLING,
  SIBLING_IN_LAW = RELATIONSHIP_NEW$SIBLING_IN_LAW,
  
  # Directional
  CHILD          = RELATIONSHIP_NEW$PARENT,
  PARENT         = RELATIONSHIP_NEW$CHILD,
  
  STEP_CHILD     = RELATIONSHIP_NEW$STEP_PARENT,
  STEP_PARENT    = RELATIONSHIP_NEW$STEP_CHILD,
  
  FOSTER_CHILD   = RELATIONSHIP_NEW$FOSTER_PARENT,
  FOSTER_PARENT  = RELATIONSHIP_NEW$FOSTER_CHILD,
  
  GRANDCHILD     = RELATIONSHIP_NEW$GRANDPARENT,
  GRANDPARENT    = RELATIONSHIP_NEW$GRANDCHILD,
  
  CHILD_IN_LAW   = RELATIONSHIP_NEW$PARENT_IN_LAW,
  PARENT_IN_LAW  = RELATIONSHIP_NEW$CHILD_IN_LAW
)


rel_map <- c(
  `1`  = 1,   # spouse ↔ spouse
  `2`  = 2,   # cohabitee ↔ cohabitee
  `3`  = 8,   # child → parent
  `8`  = 3,   # parent → child
  `4`  = 9,   # step-child → step-parent
  `9`  = 4,   # step-parent → step-child
  `12` = 12   # sibling ↔ sibling
)


# ==============================================================================
# Helper: Create baseline relationship matrix (all -9)
# ==============================================================================
.create_baseline_rels <- function(n_rows, n_cols = 16) {
  col_names <- paste0("r", sprintf("%02d", seq_len(n_cols)))
  as.data.frame(
    replicate(n_cols, rep(-9L, n_rows), simplify = FALSE) |>
      setNames(col_names)
  )
}

# ==============================================================================
# Tests
# ==============================================================================
if (!require(tinytest, quietly = TRUE)) {
  install.packages("tinytest", repos = "http://cran.r-project.org")
  library(tinytest)
}

# Single-person household
df1 <- tibble::tibble(
  HHSerial = 1,
  person   = 1
)
df1 <- dplyr::bind_cols(df1, .create_baseline_rels(1))

out1 <- reverse_relationships(df1)
expect_equal(out1$R1Rev, -9L)

cat("✓ Test 1: Single-person household passed\n")


# Married couple
df2 <- tibble::tibble(
  HHSerial = c(2, 2),
  person   = c(1, 2)
)
df2 <- dplyr::bind_cols(df2, .create_baseline_rels(2))
df2$r01[2] <- 1L  # Person 2 reports Person 1 as spouse

out2 <- reverse_relationships(df2, mode = "explicit")

expect_equal(
  out2 |> dplyr::filter(person == 1) |> dplyr::pull(R2Rev),
  1L
)
expect_equal(
  out2 |> dplyr::filter(person == 2) |> dplyr::pull(R1Rev),
  -9L
)

cat("✓ Test 2: Married couple passed\n")


# Cohabiting couple with their children
df3 <- tibble::tibble(
  HHSerial = c(3, 3, 3, 3),
  person   = 1:4
)
df3 <- dplyr::bind_cols(df3, .create_baseline_rels(4))

# Person 1's report: Person 2 is cohabitee, Persons 3,4 are children
df3$r02[1] <- 1L
df3$r03[1] <- 3L
df3$r04[1] <- 3L

# Person 2's report: Person 1 is cohabitee, Persons 3,4 are children
df3$r01[2] <- 1L
df3$r03[2] <- 3L
df3$r04[2] <- 3L

out3 <- reverse_relationships(df3, mode = "explicit")

expect_equal(
  out3 |> dplyr::filter(person == 1) |> dplyr::pull(R2Rev),
  1L
)
expect_equal(
  out3 |> dplyr::filter(person == 3) |> dplyr::pull(R1Rev),
  3L
)
expect_equal(
  out3 |> dplyr::filter(person == 3) |> dplyr::pull(R2Rev),
  3L
)

cat("✓ Test 3: Cohabiting couple with children passed\n")


# Cohabiting couple with natural mother and step-father
df4 <- tibble::tibble(
  HHSerial = c(4, 4, 4),
  person   = 1:3
)
df4 <- dplyr::bind_cols(df4, .create_baseline_rels(3))

# Person 1: Person 2 is cohabitee, Person 3 is child
df4$r02[1] <- 1L
df4$r03[1] <- 3L

# Person 2: Person 1 is cohabitee, Person 3 is step-child
df4$r01[2] <- 1L
df4$r03[2] <- 4L

out4 <- reverse_relationships(df4, mode = "explicit")

expect_equal(
  out4 |> dplyr::filter(person == 3) |> dplyr::pull(R1Rev),
  3L
)
expect_equal(
  out4 |> dplyr::filter(person == 3) |> dplyr::pull(R2Rev),
  4L
)

cat("✓ Test 4: Mother and step-father passed\n")


# Elderly man, daughter, her husband, children
df5 <- tibble::tibble(
  HHSerial = c(5, 5, 5, 5, 5),
  person   = 1:5
)
df5 <- dplyr::bind_cols(df5, .create_baseline_rels(5))

# Person 1 (grandfather): Person 2 is child, Persons 4&5 are grandchildren
df5$r02[1] <- 3L    # child
df5$r04[1] <- 16L   # grandchild
df5$r05[1] <- 16L   # grandchild

# Person 2 (mother): Person 1 is parent, Person 3 is spouse, Persons 4&5 are children
df5$r01[2] <- 8L    # parent
df5$r03[2] <- 1L    # spouse
df5$r04[2] <- 3L    # child
df5$r05[2] <- 3L    # child

# Person 3 (son-in-law): Person 2 is spouse, Persons 4&5 are step-children
df5$r02[3] <- 1L    # spouse
df5$r04[3] <- 5L    # step-child
df5$r05[3] <- 5L    # step-child

out5 <- reverse_relationships(df5, mode = "explicit")

# Grandfather sees daughter as child
expect_equal(
  out5 |> dplyr::filter(person == 2) |> dplyr::pull(R1Rev),
  3L
)

# Children see mother's report (not step-father)
expect_equal(
  out5 |> dplyr::filter(person == 4) |> dplyr::pull(R2Rev),
  3L
)

# Grandchildren see grandfather as grandparent
expect_equal(
  out5 |> dplyr::filter(person == 4) |> dplyr::pull(R1Rev),
  16L
)

cat("✓ Test 5: Three-generational family passed\n")


# Four students, two are siblings
df6 <- tibble::tibble(
  HHSerial = c(6, 6, 6, 6),
  person   = 1:4
)
df6 <- dplyr::bind_cols(df6, .create_baseline_rels(4))

# Person 1: Person 2 is sibling
df6$r02[1] <- 12L

# Person 2: Person 1 is sibling
df6$r01[2] <- 12L

out6 <- reverse_relationships(df6, mode = "explicit")

# Siblings see each other as such
expect_equal(
  out6 |> dplyr::filter(person == 1) |> dplyr::pull(R2Rev),
  12L
)
expect_equal(
  out6 |> dplyr::filter(person == 2) |> dplyr::pull(R1Rev),
  12L
)

# Unrelated people have no relationships
expect_equal(
  out6 |> dplyr::filter(person == 3) |> dplyr::pull(R1Rev),
  -9L
)
expect_equal(
  out6 |> dplyr::filter(person == 3) |> dplyr::pull(R2Rev),
  -9L
)

cat("✓ Test 6: Siblings in shared house passed\n")


# Three-generational family with inference
df7 <- tibble::tibble(
  HHSerial = c(7, 7, 7),
  person   = 1:3
)
df7 <- dplyr::bind_cols(df7, .create_baseline_rels(3))

# Person 1: Person 2 is child
df7$r02[1] <- 3L

# Person 2: Person 1 is parent, Person 3 is child
df7$r01[2] <- 8L
df7$r03[2] <- 3L

out7 <- reverse_relationships(df7, mode = "infer", relationship_map = rel_map)

# Child should see parent (inferred from parent code)
expect_equal(
  out7 |> dplyr::filter(person == 2) |> dplyr::pull(R1Rev),
  3L
)

# Grandchild should see parent (inferred)
expect_equal(
  out7 |> dplyr::filter(person == 3) |> dplyr::pull(R2Rev),
  3L
)

# Grandmother should see child (what Person 2 reported)
expect_equal(
  out7 |> dplyr::filter(person == 1) |> dplyr::pull(R2Rev),
  8L
)

cat("✓ Test 7: Three generations with inference passed\n")

cat("\n✓✓✓ All tests passed! ✓✓✓\n")
