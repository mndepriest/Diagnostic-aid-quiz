# diagnostic_app #### 

# August 11 2026
# R 4.5.2



# Set up #### 

# # For during deployment 
# source("src/Setup.R")
# source("src/04_diagnostic_filtering.R")
# 
# diag <- read.csv("data/soybean_diagnostics.csv")

# For local testing
source("src/Setup.R")
source("src/04_diagnostic_filtering.R")

base.dir <- here(here())

# The sheet with the symptoms
diag <- read.csv(
  "data/soybean_diagnostics.csv"
)

# check on the number of options for each question
# symptom_counts <- diag %>%
#   select(-disease, -growth_stage, -state) %>%
#   pivot_longer(
#     everything(),
#     names_to = "question",
#     values_to = "symptom"
#   ) %>%
#   separate_longer_delim(symptom, delim = ";") %>%
#   mutate(symptom = trimws(symptom)) %>%
#   filter(!is.na(symptom),
#          symptom != "") %>%
#   count(question, symptom, sort = TRUE)


# The sheet with disease information
info <- read.csv(
  "data/soybean_information.csv"
)



# UI #### 

ui <- fluidPage(
  
  titlePanel("Soybean Disease Diagnostic Aid"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      selectInput(
        "q1",
        "Growth stage",
        choices = c(
          "V1", "V2", "V3", "V4", "V5", "V6", "V7+",
          "R1", "R2", "R3", "R4", "R5", "R6", "R7", "R8+"
        ),
        selected = ""
      ),
      
      selectInput(
        "q2",
        "State",
        choices = sort(unique(
          trimws(unlist(strsplit(diag$state, ";")))
        )),
        selected = NULL
      ),
      
      checkboxGroupInput(
        "q3",
        "Universal symptoms",
        choices = sort(unique(
          trimws(unlist(strsplit(diag$universal_symptoms, ";")))
        ))
      ),
      
      checkboxGroupInput(
        "q4",
        "Affected plant parts",
        choices = sort(unique(
          trimws(unlist(strsplit(diag$plant_parts, ";")))
        ))
      ),
      
      # The questions that rely on plant parts
      uiOutput("q5_ui"),
      uiOutput("q6_ui"),
      
      uiOutput("q7_ui"),
      uiOutput("q8_ui"),
      uiOutput("q9_ui"),
      
      uiOutput("q10_ui"),
      uiOutput("q11_ui"),
      
      uiOutput("q12_ui"),
      
      # Button for resetting
      actionButton(
        "reset",
        "Reset questions"
      )
      
    ),
    
    mainPanel(
      
      h1("Possible diagnoses:"),
      
      uiOutput("possible_diseases"),
    )
  )
)



# Server #### 

server <- function(input, output, session) {
  
  possible <- reactive({
    
    q1 <- if (input$q1 == "") NULL else input$q1
    q2 <- if (input$q2 == "") NULL else input$q2
    
    # See which questions are active
    active <- get_next_questions(
      q4 = input$q4,
      q5 = input$q5,
      q7 = input$q7,
      q10 = input$q10,
      q12 = input$q12
    )
    
    result <- get_possible_diseases(
      diag,
      q1 = q1,
      q2 = q2,
      q3 = input$q3,
      q4 = input$q4,
      
      q5  = if (5  %in% active) input$q5  else NULL,
      q6  = if (6  %in% active) input$q6  else NULL,
      q7  = if (7  %in% active) input$q7  else NULL,
      q8  = if (8  %in% active) input$q8  else NULL,
      q9  = if (9  %in% active) input$q9  else NULL,
      q10 = if (10 %in% active) input$q10 else NULL,
      q11 = if (11 %in% active) input$q11 else NULL,
      q12 = if (12 %in% active) input$q12 else NULL
    ) %>%
      left_join(
        info %>% select(disease, description, image1, image1_citation, image2, image2_citation),
        by = "disease"
      )
    
    
    # Explicitly result the resulting diseases
    return(result)
  })
  
  next_questions <- reactive({
    
    get_next_questions(
      q4 = input$q4,
      q5 = input$q5,
      q7 = input$q7,
      q10 = input$q10,
      q12 = input$q12
    )
    
  })
  
  output$possible_diseases <- renderUI({
    
    diseases <- possible()
    
    tagList(
      lapply(seq_len(nrow(diseases)), function(i) {
        
        tagList(
          
          h3(diseases$disease[i]),
          
          p(diseases$description[i]),
          
          fluidRow(
            
            column(
              width = 6,
              
              tags$img(
                src = paste0(
                  "soybean/",
                  diseases$image1[i]
                ),
                style = "width: 100%; height: auto;"
              ),
              
              tags$small(
                diseases$image1_citation[i]
              )
            ),
            
            column(
              width = 6,
              
              tags$img(
                src = paste0(
                  "soybean/",
                  diseases$image2[i]
                ),
                style = "width: 100%; height: auto;"
              ),
              
              tags$small(
                diseases$image2_citation[i]
              )
            )
          ),
          
          tags$hr()
        )
      })
    )
  })

  
  output$diagnosis_count <- renderText({
    
    paste(
      "Number of possible diagnoses:",
      nrow(possible())
    )
    
  })
  
  output$q5_ui <- renderUI({
    
    req(input$q4)
    
    if ("leaves" %in% input$q4) {
      
      checkboxGroupInput(
        "q5",
        "Leaf symptoms",
        choices = sort(unique(
          trimws(unlist(strsplit(diag$leaf_symptoms, ";")))
        ))
      )
      
    }
    
  })
  
  
  output$q6_ui <- renderUI({
    
    req(input$q5)
    
    if ("discoloration or lesions (including chlorosis)" %in% input$q5) {
      
      checkboxGroupInput(
        "q6",
        "Leaf discoloration/lesion characteristics",
        choices = sort(unique(
          trimws(unlist(strsplit(
            diag$leaf_discoloration.lesion_characteristics,
            ";"
          )))
        ))
      )
      
    }
    
  })
  
  
  output$q7_ui <- renderUI({
    
    req(input$q4)
    
    if ("stems/petioles" %in% input$q4) {
      
      checkboxGroupInput(
        "q7",
        "Stem/petiole symptoms",
        choices = sort(unique(
          trimws(unlist(strsplit(
            diag$stem.petiole_symptoms,
            ";"
          )))
        ))
      )
      
    }
    
  })
  
  
  output$q8_ui <- renderUI({
    
    req(input$q7)
    
    if ("external discoloration or lesions" %in% input$q7) {
      
      checkboxGroupInput(
        "q8",
        "External stem/petiole discoloration/lesion characteristics",
        choices = sort(unique(
          trimws(unlist(strsplit(
            diag$stem.petiole_external_discoloration.lesion_characteristics,
            ";"
          )))
        ))
      )
      
    }
    
  })
  
  
  output$q9_ui <- renderUI({
    
    req(input$q7)
    
    if ("internal discoloration or lesions" %in% input$q7) {
      
      checkboxGroupInput(
        "q9",
        "Internal stem/petiole discoloration/lesion characteristics",
        choices = sort(unique(
          trimws(unlist(strsplit(
            diag$stem_petiole_internal_discoloration.lesion_characteristics,
            ";"
          )))
        ))
      )
      
    }
    
  })
  
  output$q10_ui <- renderUI({
    
    req(input$q4)
    
    if ("roots" %in% input$q4) {
      
      checkboxGroupInput(
        "q10",
        "Root symptoms",
        choices = sort(unique(
          trimws(unlist(strsplit(diag$root_symptoms, ";")))
        ))
      )
      
    }
    
  })
  
  
  output$q11_ui <- renderUI({
    
    req(input$q10)
    
    if ("discoloration or lesions" %in% input$q10) {
      
      checkboxGroupInput(
        "q11",
        "Root discoloration/lesion characteristics",
        choices = sort(unique(
          trimws(unlist(strsplit(
            diag$root_discoloration.lesion_characteristics,
            ";"
          )))
        ))
      )
      
    }
    
  })
  
  
  output$q12_ui <- renderUI({
    
    req(input$q4)
    
    if ("pods/seeds" %in% input$q4) {
      
      checkboxGroupInput(
        "q12",
        "Pod/seed symptoms",
        choices = sort(unique(
          trimws(unlist(strsplit(diag$pod.seed_symptoms, ";")))
        ))
      )
      
    }
    
  })
  
  
  
  # Clear answers
  observeEvent(input$reset, {
    
    updateSelectInput(
      session,
      "q1",
      selected = ""
    )
    
    updateSelectInput(
      session,
      "q2",
      selected = ""
    )
    
    updateCheckboxGroupInput(
      session,
      "q3",
      selected = character(0)
    )
    
    updateCheckboxGroupInput(
      session,
      "q4",
      selected = character(0)
    )
    
    # It shouldn't be necessary to clear out the dependent questions, but just in case
    updateCheckboxGroupInput(session, "q5", selected = character(0))
    updateCheckboxGroupInput(session, "q6", selected = character(0))
    updateCheckboxGroupInput(session, "q7", selected = character(0))
    updateCheckboxGroupInput(session, "q8", selected = character(0))
    updateCheckboxGroupInput(session, "q9", selected = character(0))
    updateCheckboxGroupInput(session, "q10", selected = character(0))
    updateCheckboxGroupInput(session, "q11", selected = character(0))
    updateCheckboxGroupInput(session, "q12", selected = character(0))
    
  })
}


# Execution #### 

shinyApp(ui, server)