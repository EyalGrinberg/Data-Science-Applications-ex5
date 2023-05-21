library(tidymodels)
library(pROC)
library(xgboost)
library(readr)
#data <- read_csv("C:/Users/user/OneDrive - mail.tau.ac.il/Desktop/R/data science apps/HW5-EyalGrinberg/data/bank_marketing.csv")
# setting data path for external usage
curr_working_dir_path <- getwd()
data_desc_path<- paste0(dirname(curr_working_dir_path), "/data/bank_marketing_features_desc.txt")
data_path <- paste0(dirname(curr_working_dir_path), "/data/bank_marketing.csv")
data <- read_csv(data_path)
library(shiny)

model_path <- paste0(dirname(curr_working_dir_path), "/boost_tree_model.rds")
# Load the model
model <- readRDS(model_path)#"C:/Users/user/OneDrive - mail.tau.ac.il/Desktop/R/data science apps/HW5-EyalGrinberg/boost_tree_model.rds")


# Define the UI
ui <- fluidPage(
  
  navbarPage("zoltar!",
             
             tabPanel("Home",
                      # Input values
                      sidebarPanel(
                        HTML("<h3>Input parameters</h3>"),
      # Categorial features
      selectInput("job", "job:", choices = c("housemaid", "services", "admin.", "blue-collar", "technician", "retired", "management", "unemployed", "self-employed", "unknown", "entrepreneur", "student"), selected = names(table(data$job))[which.max(table(data$job))]),
      selectInput("marital", "marital:", choices = c("married", "single", "divorced", "unknown"), selected = names(table(data$marital))[which.max(table(data$marital))]),
      selectInput("education", "education:", choices = c("basic.4y", "high.school", "basic.6y", "basic.9y", "professional.course", "unknown", "university.degree", "illiterate"), selected = names(table(data$education))[which.max(table(data$education))]),
      selectInput("default", "default:", choices = c("no", "unknown", "yes"), selected = names(table(data$default))[which.max(table(data$default))]),
      selectInput("housing", "housing:", choices = c("no", "yes", "unknown"), selected = names(table(data$housing))[which.max(table(data$housing))]),
      selectInput("loan", "loan:", choices = c("no", "yes", "unknown"), selected = names(table(data$loan))[which.max(table(data$loan))]),
      selectInput("contact", "contact:", choices = c("telephone", "cellular"), selected = names(table(data$contact))[which.max(table(data$contact))]),
      selectInput("month", "month:", choices = c("may", "jun", "jul", "aug", "oct", "nov", "dec", "mar", "apr", "sep"), selected = names(table(data$month))[which.max(table(data$month))]),
      selectInput("day_of_week", "day_of_week:", choices = c("mon", "tue", "wed", "thu", "fri"), selected = names(table(data$day_of_week))[which.max(table(data$day_of_week))]),
      selectInput("poutcome", "poutcome:", choices = c("nonexistent", "failure", "success"), selected = names(table(data$poutcome))[which.max(table(data$poutcome))]),

      # Continuous features
      # code for extracting the features description is commented out after the model section
      sliderInput("age", "age:", min = 17, max = 98, value = median(data$age)),
      sliderInput("campaign", "campaign:", min = 1, max = 56, value = median(data$campaign)),
      sliderInput("previous", "previous:", min = 0, max = 7, value = median(data$previous)),
      sliderInput("cons.price.idx", "cons.price.idx:", min = 92.201, max = 94.76, value = median(data$cons.price.idx)),
      sliderInput("cons.conf.idx", "cons.conf.idx:", min = -50.8, max = -26.9, value = median(data$cons.conf.idx)),
      sliderInput("euribor3m", "euribor3m:", min = 0.634, max = 5.045, value = median(data$euribor3m)),
      
      actionButton("predictBtn", "Predict", class = "btn btn - primary")
    ),
    mainPanel(
     
          # tab panel 1
          tags$label(h3('Status/Output')), # Status/Output Text Box
          verbatimTextOutput('contents'),
          tableOutput('tabledata') # Prediction results table
    )
      ), 
      
      tabPanel("About",
               mainPanel(
               textOutput("fileContent") 
              )
      ),
             
     tabPanel("score prediction",
              sidebarPanel(HTML("<h3>select names</h3>"),selectInput("name1","name1:", choices = c("yam","eyal")),
                           selectInput("name2","name2:", choices = c("eyal","yam")),
                           actionButton("predictBtn1", "Predict", class = "btn btn - primary")),
                       mainPanel(HTML("<h3> score :</h3>"),
                         
                         verbatimTextOutput("fileContent1"),
                         
                         
                         
                         
                       )
              )
              
      ) 
    )
  



# Define the server
server <- function(input, output, session) {
  
  datasetInput <- reactive({
  
    # Create a data frame with the input values
    df <- data.frame(
      # categorical
      job = input$job,
      marital = input$marital,
      education = input$education,
      default = input$default,
      housing = input$housing,
      loan = input$loan,
      contact = input$contact,
      month = input$month,
      day_of_week = input$day_of_week,
      poutcome = input$poutcome,
      # continuous
      age = input$age,
      campaign = input$campaign,
      previous = input$previous,
      cons.price.idx = input$cons.price.idx,
      cons.conf.idx = input$cons.conf.idx,
      euribor3m = input$euribor3m
    )
    
    write.table(df,"df.csv", sep=",", quote = FALSE, row.names = FALSE, col.names = TRUE)
    
    test <- read.csv(paste("df", ".csv", sep=""), header = TRUE)
    Output <- data.frame(Prediction=predict(model,test), round(predict(model,test,type="prob"), 3))
    print(Output)
    
  })
  
  # Status/Output Text Box
  output$contents <- renderPrint({
    if (input$predictBtn>0) { 
      isolate("Calculation complete.") 
    } else {
      return("Server is ready for calculation.")
    }
  })
  
  # Prediction results table
  output$tabledata <- renderTable({
    if (input$predictBtn>0) { 
      isolate(datasetInput()) 
    } 
  })
  ### tab2
  output$fileContent <- renderText({
    content <- readLines(data_desc_path)
    content
  })
  ###### tab3
  output$fileContent1 <- renderPrint({
    input_sum <- ifelse(input$name1 == input$name2,0,1)
    input_sum <- input_sum +as.numeric(input$predictBtn1>0)
    if (input_sum== 2 ) { 
      isolate("100, great job! im amazed:)") 
    } else {
      return("pls choose the two name and click on the predict .")
    }
  })
}

# Run the app
shinyApp(ui, server)