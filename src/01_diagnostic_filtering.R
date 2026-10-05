# 04_diagnostic_filtering #### 

# Aug 10 2026
# R 4.5.2



# Set up #### 

# Load packages and environment objects
source("src/Setup.R")

# Diagnostic criteria
diag <- read.csv('data/soybean_diagnostics.csv')



# filter_diag() to fix no diagnoses when quiz is reset #### 

filter_diag <- function(data, question, answers, match = "any") {
  
  if (is.null(answers) || length(answers) == 0) {
    return(data)
  }
  
  if (nrow(data) == 0) {
    return(data)
  }
  
  data %>%
    filter(
      sapply(
        .data[[question]],
        function(x) {
          
          if (is.na(x)) return(FALSE)
          
          available <- trimws(strsplit(x, ";")[[1]])
          
          if (match == "any") {
            any(answers %in% available)
          } else {
            all(answers %in% available)
          }
        }
      )
    )
}



# get_next_questions() ####

get_next_questions <- function(q4,
                               q5 = NULL,
                               q7 = NULL,
                               q10 = NULL,
                               q12 = NULL) {
  
  questions <- c()
  
  # Leaves
  if ("leaves" %in% q4) {
    questions <- c(questions, 5)
    
    if ("discoloration or lesions (including chlorosis)" %in% q5) {
      questions <- c(questions, 6)
    }
  }
  
  # Stems/petioles
  if ("stems/petioles" %in% q4) {
    questions <- c(questions, 7)
    
    if ("external discoloration or lesions" %in% q7) {
      questions <- c(questions, 8)
    }
    
    if ("internal discoloration or lesions" %in% q7) {
      questions <- c(questions, 9)
    }
  }
  
  # Roots
  if ("roots" %in% q4) {
    questions <- c(questions, 10)
    
    if ("discoloration or lesions" %in% q10) {
      questions <- c(questions, 11)
    }
  }
  
  # Pods/seeds
  if ("pods/seeds" %in% q4) {
    questions <- c(questions, 12)
    
  }
  
  questions
}




# get_possible_diseases() #### 

get_possible_diseases <- function(diag,
                                  q1 = NULL,
                                  q2 = NULL,
                                  q3 = NULL,
                                  q4 = NULL,
                                  q5 = NULL,
                                  q6 = NULL,
                                  q7 = NULL,
                                  q8 = NULL,
                                  q9 = NULL,
                                  q10 = NULL,
                                  q11 = NULL,
                                  q12 = NULL) {
  
  possible <- diag
  
  # FIRST 4 QUESTIONS
  if (!is.null(q1)) {
    possible <- filter_diag(possible, "growth_stage", q1, match = "all")
  }
  if (!is.null(q2)) {
    possible <- filter_diag(possible, "state", q2, match = "all")
  }
  if (!is.null(q3)) {
    possible <- filter_diag(possible, "universal_symptoms", q3, match = "any")
  }
  if (!is.null(q4)) {
    possible <- filter_diag(possible, "plant_parts", q4, match = "any")
  }
  
  # LEAVES
  # Do leaf questions get asked?
  if (!is.null(q5)) {
    possible <- filter_diag(
      possible,
      "leaf_symptoms",
      q5,
      match = "any"
    )
  }
  
  # Do leaf discoloration/lesion questions get asked?
  if (!is.null(q6)) {
    possible <- filter_diag(
      possible,
      "leaf_discoloration.lesion_characteristics",
      q6,
      match = "any"
    )
  }
  
  # STEMS/PETIOLES
  # Do stem/petiole questions get asked?
  if (!is.null(q7)) {
    possible <- filter_diag(
      possible,
      "stem.petiole_symptoms",
      q7,
      match = "any"
    )
  }
  
  # Do stem/petiole external discoloration/lesion questions get asked?
  if (!is.null(q8)) {
    possible <- filter_diag(
      possible,
      "stem.petiole_external_discoloration.lesion_characteristics",
      q8,
      match = "any"
    )
  }
  
  # Do stem/petiole internal discoloration/lesion questions get asked?
  if (!is.null(q9)) {
    possible <- filter_diag(
      possible,
      "stem_petiole_internal_discoloration.lesion_characteristics",
      q9,
      match = "any"
    )
  }
  
  # ROOTS
  # Do root questions get asked?
  if (!is.null(q10)) {
    possible <- filter_diag(
      possible,
      "root_symptoms",
      q10,
      match = "any"
    )
  }
  
  # Do root discoloration/lesion questions get asked?
  if (!is.null(q11)) {
    possible <- filter_diag(
      possible,
      "root_discoloration.lesion_characteristics",
      q11,
      match = "any"
    )
  }
  
  # PODS/SEEDS
  # Do pod/seed questions get asked?
  if (!is.null(q12)) {
    possible <- filter_diag(
      possible,
      "pod.seed_symptoms",
      q12,
      match = "any"
    )
  }

  
  return(possible)
}


