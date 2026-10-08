# Standardising organism names

organism_spellings <- rbind(
  data.frame(name = "negative",
    spelling = c(
      "Culture Negative (PD Use Only)", "culture negative", "culture-negative",
      "negative", "no growth", "culture negative peritonitis", "sterile")),
  data.frame(name = "S. epidermidis",
    spelling = c(
      "Coag - Neg Staph, Staph Epidermidis", "Staphylococcus epidermidis",
      "Staph epidermidis", "S epidermidis", "CoNS epidermidis")),
  data.frame(name = "Coag-neg Staph (other)",
    spelling = c(
      "Coag - Neg Staph, Other (Specify)", "Coagulase negative staph other",
      "CoNS other")),
  data.frame(name = "Coag-neg Staph",
    spelling = c(
      "Coag - Neg Staph, Unknown", "Coagulase negative Staphylococcus",
      "Coagulase negative Staphylococci", "Coagulase negative staph",
      "Coag neg staph", "CoNS", "CNS staph")),
  data.frame(name = "S. aureus (MRSA)",
    spelling = c(
      "Staphylococcus Aureus, Methicillin Resistant", "MRSA", "S. aureus MRSA",
      "Staphylococcus aureus MRSA",
      "Methicillin resistant Staphylococcus aureus", "Staph aureus MRSA")),
  data.frame(name = "S. aureus (non-MRSA)",
    spelling = c(
      "Staphylococcus Aureus, Non-Mrsa", "MSSA", "S. aureus non MRSA",
      "Staphylococcus aureus non-MRSA",
      "Methicillin sensitive Staphylococcus aureus",
      "Methicillin susceptible Staphylococcus aureus")),
  data.frame(name = "S. aureus",
    spelling = c(
      "Staphylococcus Aureus, Unknown Sensitive", "Staphylococcus aureus",
      "Staph aureus", "S aureus", "Staph. aureus",
      "Staphylococcus aureus unknown")),
  data.frame(name = "Strep viridans group",
    spelling = c(
      "Streptococcus Viridians Group (Sangius, Bovis, Etc)",
      "Streptococcus viridans", "Strep viridans", "Viridans streptococci",
      "Viridans group streptococci", "Streptococcus bovis", "S. bovis",
      "Streptococcus sanguinis", "S. sanguinis", "Streptococcus mitis")),
  data.frame(name = "Streptococcus (other)",
    spelling = c(
      "Streptococcus, Other (Specify)")),
  data.frame(name = "Streptococcus",
    spelling = c("Streptococcus, Unknown",
      "Strep", "Streptococci", "Streptococcus species", "Strep species")),
  data.frame(name = "Enterococcus",
    spelling = c(
      "Enterococcus (Strep. Faecalis/Faecium)", "Enterococcus faecalis",
      "E. faecalis", "Enterococcus faecium", "E. faecium",
      "Streptococcus faecalis", "Streptococcus faecium", "Enterococci")),
  data.frame(name = "Diphtheroids",
    spelling = c(
      "Diptheroids (Corynebacteria)", "Diphtheroids", "Corynebacterium",
      "Corynebacteria")),
  data.frame(name = "Gram-positive (other)",
    spelling = c(
      "Gram Positive Organism, Other (Specify)")),
  data.frame(name = "Gram-positive",
    spelling = c(
      "Gram Positive Organism, Unknown", "Gram positive",
      "Gram positive cocci", "Gram positive organism", "Gram positive bacilli",
      "GPC")),
  data.frame(name = "P. aeruginosa",
    spelling = c("Pseudomonas Aeruginosa",
      "Pseudomonas aeruginosa", "Ps aeruginosa", "P aeruginosa")),
  data.frame(name = "Pseudomonas (other)",
    spelling = c(
      "Pseudomonas, Other (Specify)")),
  data.frame(name = "Pseudomonas",
    spelling = c("Pseudomonas, Unknown",
      "Pseudomonas species")),
  data.frame(name = "E. coli",
    spelling = c("E. Coli", "Escherichia coli",
      "E coli")),
  data.frame(name = "Klebsiella",
    spelling = c("Klebsiella Sp",
      "Klebsiella pneumoniae", "K. pneumoniae", "Klebsiella oxytoca",
      "K. oxytoca")),
  data.frame(name = "Enterobacter",
    spelling = c("Enterobacter Species",
      "Enterobacter cloacae", "E. cloacae", "Enterobacter aerogenes")),
  data.frame(name = "Serratia",
    spelling = c("Serratia Species",
      "Serratia marcescens", "S. marcescens")),
  data.frame(name = "Proteus",
    spelling = c("Proteus Species",
      "Proteus mirabilis", "P. mirabilis")),
  data.frame(name = "Neisseria",
    spelling = c("Neisseria Sp")),
  data.frame(name = "Gram-negative (other)",
    spelling = c(
      "Gram Negative Organisms, Other (Specify)")),
  data.frame(name = "Gram-negative",
    spelling = c(
      "Gram Negative Organisms, Unknown", "Gram negative",
      "Gram negative organism", "Gram negative bacilli", "Gram negative rods",
      "GNR")),
  data.frame(name = "Anaerobes",
    spelling = c("Anaerobic Bacteria",
      "Anaerobe", "Anaerobic")),
  data.frame(name = "C. albicans",
    spelling = c("Candida Albicans",
      "Candida albicans", "C albicans")),
  data.frame(name = "Candida (other)",
    spelling = c(
      "Candida, Other (Specify)", "Candida", "Candida species",
      "Candida glabrata", "C. glabrata", "Candida parapsilosis",
      "C. parapsilosis", "Candida tropicalis")),
  data.frame(name = "Fungi/Yeast (other)",
    spelling = c(
      "Fungi/Yeast, Other (Specify)", "Fungi", "Yeast", "Fungus",
      "Fungi yeast", "Yeast other")),
  data.frame(name = "M. tuberculosis",
    spelling = c(
      "Mycobacterium, Tuberculosis", "Mycobacterium tuberculosis", "TB",
      "Tuberculosis")),
  data.frame(name = "No culture taken",
    spelling = c("No Culture Taken",
      "No culture", "Culture not taken", "Not cultured", "No cultures")),
  data.frame(name = "Other organism",
    spelling = c(
      "Other Organism (Specify)", "Other"))
)





#' Reduce an organism spelling to a comparable key (eg. "E. coli", "escherichia coli" and E.coli " share a key)
#' @noRd
organism_key <- function(x) {
  x <- tolower(as.character(x))
  x <- gsub("[^a-z0-9]+", " ", x)
  x <- gsub("\\b(sp|spp|species)\\b", " ", x, perl = TRUE)
  trimws(gsub("\\s+", " ", x))
}




#' Standard names, in the order of the table
#' @noRd
organism_names <- unique(organism_spellings$name)




#' Named vector mapping each accepted organism key to its standard name
#' @noRd
organism_lookup <- c(organism_spellings$name, organism_names)
names(organism_lookup) <- organism_key(c(organism_spellings$spelling, organism_names))
organism_lookup <- organism_lookup[!duplicated(names(organism_lookup))]




#' Look up the standard name for one organism spelling
#' @noRd
match_organism <- function(x) {
  unname(organism_lookup[organism_key(x)])
}




#' Standardise the organisms named in one cell
#' @noRd
standardise_organism_cell <- function(cell) {
  whole <- match_organism(cell)
  if (!is.na(whole)) {
    return(list(std = whole, unknown = character(0)))
  }
  # a cell may have several organisms separated by commas, semicolons, "/", "&", "+" or "and"
  chunks <- trimws(strsplit(cell, "[;&+/]|\\band\\b", perl = TRUE)[[1]])
  chunks <- chunks[nzchar(chunks)]

  std <- character(0)
  unknown <- character(0)
  for (chunk in chunks) {
    parts <- trimws(strsplit(chunk, ",", fixed = TRUE)[[1]])
    parts <- parts[nzchar(parts)]
    i <- 1L
    while (i <= length(parts)) {
      for (k in seq(min(4L, length(parts) - i + 1L), 1L)) {
        cand <- paste(parts[i:(i + k - 1L)], collapse = ", ")
        hit <- match_organism(cand)
        if (!is.na(hit) || k == 1L) {
          if (is.na(hit)) unknown <- c(unknown, parts[i]) else std <- c(std, hit)
          i <- i + k
          break
        }
      }
    }
  }

  list(std = unique(std), unknown = unknown)
}




#' Standardise the organism column of the PE file, log unrecognised names
#' @noRd
check_organisms <- function(df, what, log, col = "organism", header_rows = 1L) {
  stopifnot(is.data.frame(df), col %in% names(df))
  if (nrow(df) == 0) {
    return(df)
  }

  cells <- as.character(df[[col]])
  std <- cells
  bad <- integer(0)

  for (i in seq_along(cells)) {
    if (is.na(cells[i]) || !nzchar(trimws(cells[i]))) next
    res <- standardise_organism_cell(cells[i])
    if (length(res$unknown) > 0) {
      bad <- c(bad, i)
      log$add("The ", what, " file, row ", sheet_row(df, i, header_rows),
              ": organism ", paste0("`", res$unknown, "`", collapse = ", "),
              " is not in the accepted organism list; row excluded from the unit.")
    } else {
      std[i] <- paste(res$std, collapse = ", ")
    }
  }

  df[[col]] <- std
  if (length(bad) > 0) df <- df[-bad, , drop = FALSE]
  df
}
