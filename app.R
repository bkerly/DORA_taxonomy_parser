# The purpose of this app is to allow you to upload
# DORA provider database files, check some general categories of provider (taxonomies) you want,
# and then download a list of emails as a csv.
# 
# (It doesn't have to be just emails, but that's how it's set up now.)
# 
# Input: taxonomy_checklist (included)
#         DORA provider directory (user upload)
#         User selections (in-app)
#         
# Output: a downloadable file of provider emails
# 
# by Brian Erly, brian.erly@state.co.us, brianerly@gmail.com


library(shiny)
library(tidyverse)
library(reshape2)

# This makes it so you can upload real big files, up to 1000mb. Bigger than that and we'll have to update something, but it might also break other things.
options(shiny.maxRequestSize = 1000 * 1024^2)

# This loads data about which specialties fall under which taxonomatic categories
# For example, family medicine providers are vaccine provdiers and care for pregnant women.
taxonomy_checklist <- read_csv("data/taxonomy_checklist.csv") %>%
  select(-Count) %>%
  pivot_longer(-Specialty) %>%
  filter(value == 1)

# This creates a list of unique choices (for a checklist later in the app)
taxonomy_choices <- unique(taxonomy_checklist$name)


# Create functions to parse the DORA list ---------------------------------

# This loads some default DORA data to play with.
# Will not be able to share this, but it's nice to have now for debugging.
dora_data <- read_csv("data/DPO_Health_Alert_Network_CDPHE.csv")
# This is a function which converts the DORA file into a list of providers and their specialties
# Every provider gets listed once for each specialty they hold, one specialty per line.
parse_specialties <- function(input = data)
  parse_specialties <- function(input = data)
  {
    # Here's the format:
    # (Certification) Internal Medicine, (Certification) Pediatrics, (Certification) Preventive Medicine: Addiction Medicine, (Specialty) Emergency Medicine, (Specialty) Internal Medicine 
    
    data2 <- data %>%
      mutate(cert_spec = case_when(
        cred_type == "APN" ~ "(Specialty) Nurse Practitioner",
        cred_type == "PA" ~ "(Specialty) Physician Assistant",
        cred_type == "PHA" ~ "(Specialty) Pharmacist",
        TRUE ~ cert_spec
      )) %>%
      select(-cred_type) %>%
      separate(cert_spec, into=c("a","b","c","d","e","f","g","h","i","j","k","l","m"),
               sep = ", ") %>%
      group_by(cred_num) %>%
      slice(1) %>%
      ungroup()
    
    
    data3 <- data2 %>%
      pivot_longer(-c(cred_num,email)) %>%
      filter(!is.na(value)) %>%
      mutate(specialty = 
               startsWith(value, prefix = "(Specialty)"),
             certification = 
               startsWith(value, prefix = "(Certification)")
      ) %>%
      mutate(value = gsub(".*) ", "", value))
    
    data_spec <- data3 %>%
      filter(specialty) %>%
      filter(value != "Other") %>%
      group_by(cred_num,email) %>%
      summarize(specialties = list(value)) %>%
      ungroup()
    
    return(data_spec)
  }



df <- parse_specialties() 



# Return emails from a selection of specialties ---------------------------



# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("DORA Taxonomy Parser"),

    # Sidebar with checkboxes for different specialties
    sidebarLayout(
        sidebarPanel(
          checkboxGroupInput(inputId = "taxonomies", 
                        label = "1. Select Taxonomies", 
                        choices = taxonomy_choices
                        )
        ),

        # Main panel
        mainPanel(
          # Upload thing
          fileInput("upload", "2. Upload DORA file"),
          
          # Show a preview of the export file
          tableOutput("preview"),
          
          # List the selected taxonomies (if needed, good for debuggin')
           textOutput("text"),
          
          # Download the file
          downloadButton("download", "3. Download Email List")
          
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  # Convert the checked taxonomies into a list of specialties
  selected_specialties <- reactive({
    taxonomy_checklist %>%
      filter(name %in% input$taxonomies) %>%
      select(Specialty) %>%
      unlist()
  })

  dora_specialty_data <- reactive({
    req(input$upload)
    parse_specialties(dora_data_input = read_csv(input$upload$datapath)) %>%
     filter(specialties %in% selected_specialties()) %>%
     group_by(email) %>%
     summarize(specialties = paste(specialties,collapse = "; ")) 
  })
  
  # This is OK for debugging but really not needed all the time. 
  # output$text <- renderPrint({
  #   paste0(input$taxonomies)
  #   })
  
  output$preview <- renderTable({

    dora_specialty_data() %>%
      head()
  })
  
  output$download <- downloadHandler(
    filename = function(){paste0("HAN provider list",Sys.Date(),".csv")},
    content = function(file){
      write_csv(dora_specialty_data(),file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
