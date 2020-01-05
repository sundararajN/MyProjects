#
# This is a Shiny web application for the Machine Learning project created. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(DT)

# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Machine Learning Approaches on the Heart Study Dataset"),
   
   tags$br(),
   
   # Sidebar with a slider input for number of bins 
   #sidebarLayout(
      #sidebarPanel(
        
         fluidPage(
           
           fluidRow(
             
             column(width = 3, fileInput("Heart_Study_File", label = "Upload File", accept = c(
               
               "text/csv",
               "text/comma-seperated-values",
               ".csv"))),
             
             column(width = 9, dataTableOutput("table"))
          ),
           
           fluidRow(
             
             column(width = 10, radioButtons("ML_Method", label = "Choose a Machine Learning Approach",choices = c("KNN","Decision Tree","Logistic Regression","SVM"),selected = c("KNN"))
                                            
                  )
             
                ),
           
           fluidRow(
             
             column(width = 6, actionButton("start",label = "Execute"))
             
           ),
          
          tags$br(),
            
        #)
         
    #),
         
      
      mainPanel(
        
        tabsetPanel(
          
          tabPanel("Processed Data",dataTableOutput("data_Table")),
          tabPanel("Machine Learning Output",verbatimTextOutput("out_summary"))
        
        )
         
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  reactive_D <- eventReactive(input$start,{
    
    infile <- input$Heart_Study_File 
    if(is.null(infile)){
      
      return(NULL)
      
    }
    
    infile
  
  })
  
  reactive_Data_exp <- reactive({
  
          read.csv(reactive_D()$datapath)
  
  })
  
  
  reactive_Data <- reactive({
    
    
    Data_File <- Modify_Data(data = reactive_Data_exp())
    Data_File <- Data_Preparation(data = Data_File)
    Data_File
    
  })
  

  output$table <- renderDataTable({
      
    reactive_D()
    
    
  },options = list(paging = FALSE,dom = 't'))
  
  output$data_Table <- renderDataTable({
    
    reactive_Data()
    
  },options = list(dom = 't'))
  
  output$out_summary <- renderPrint({
    
    Data_L <- Partition_Data(data = reactive_Data())

        cat("For Machine Learning Approach:",input$ML_Method,"\n\n")
    
    if(input$ML_Method == "KNN"){

      cat("Evaluation Metrics:","\n")
      KNN_L <- Model_Construction_KNN(Train_Data = Data_L$train_Data,Test_Data = Data_L$test_data)
      cat(unlist(names(KNN_L$Conf_Matrix[1])),"\n")
      cat(unlist(KNN_L$Conf_Matrix[1][1]),"\n\n")
      cat(" ",paste0(rownames(KNN_L$Conf_Matrix[2]$table),collapse = " "),"\n")
      cat("0",paste0(KNN_L$Conf_Matrix[2]$table[1,],collapse = " "),"\n")
      cat("1",paste0(KNN_L$Conf_Matrix[2]$table[2,],collapse = " "),"\n\n")
      cat(paste0(names(KNN_L$Conf_Matrix[3]$overall),collapse = " "),"\n")
      cat(KNN_L$Conf_Matrix[3]$overall,"\t\n\n")
      cat(paste0(names(KNN_L$Conf_Matrix[4]$byClass)),"\t\n",KNN_L$Conf_Matrix[4]$byClass,"\t\n\n")
      cat("Area Under Curve Score for",input$ML_Method,":",KNN_L$AUC,"\n")

    }
    
    if(input$ML_Method == "Decision Tree" ){
      
      cat("Evaluation Metrics(10-fold cross validation):","\n")
      tree_L <- Model_Construction_Model_Tr(Train_Data = Data_L$train_Data, Test_Data = Data_L$test_data)
      cat(unlist(names(KNN_L$Conf_Matrix[1])),"\n")
      cat(unlist(tree_L$Conf_Matrix_CV[1]),"\n\n")
      cat(" ",paste0(rownames(tree_L$Conf_Matrix_CV[2]$table),collapse = " "),"\n")
      cat("0",paste0(tree_L$Conf_Matrix_CV[2]$table[1,],collapse = " "),"\n")
      cat("1",paste0(tree_L$Conf_Matrix_CV[2]$table[2,],collapse = " "),"\n\n")
      cat(paste0(names(tree_L$Conf_Matrix_CV[3]$overall),collapse = " "),"\n")
      cat(tree_L$Conf_Matrix_CV[3]$overall,"\t\n\n")
      cat(paste0(names(tree_L$Conf_Matrix_CV[4]$byClass)),"\t\n",tree_L$Conf_Matrix_CV[4]$byClass,"\t\n\n")
      cat("Area Under Curve Score for",input$ML_Method,":",tree_L$AUC)

    }
    
    if(input$ML_Method == "SVM"){
      
      cat("Evaluation Metrics(10-fold cross validation):","\n")
      SVM_L <- Model_Construction_SVM(Train_Data = Data_L$train_Data,Test_Data = Data_L$test_data)
      cat(unlist(names(SVM_L$Conf_Mat_KV[1])),"\n")
      cat(unlist(SVM_L$Conf_Mat_KV[1]),"\n\n")
      cat(" ",paste0(rownames(SVM_L$Conf_Mat_KV[2]$table),collapse = " "),"\n")
      cat("0",paste0(SVM_L$Conf_Mat_KV[2]$table[1,],collapse = " "),"\n")
      cat("1",paste0(SVM_L$Conf_Mat_KV[2]$table[2,],collapse = " "),"\n\n")
      cat(paste0(names(SVM_L$Conf_Mat_KV[3]$overall),collapse = " "),"\n")
      cat(SVM_L$Conf_Mat_KV[3]$overall,"\t\n\n")
      cat(paste0(names(SVM_L$Conf_Mat_KV[4]$byClass)),"\t\n",SVM_L$Conf_Mat_KV[4]$byClass,"\t\n\n")      
      cat("Area Under Curve Score for",input$ML_Method,":",SVM_L$AUC)

    }
    
    
    if(input$ML_Method == "Logistic Regression"){
    
    cat("Evaluation Metrics(10-fold cross validation):","\n")
    glm_L <- Model_Construction_glm(Train_Data = Data_L$train_Data, Test_Data = Data_L$test_data)
    cat(unlist(names(glm_L$Conf_Matrix_CV[1])),"\n")
    cat(unlist(glm_L$Conf_Matrix_CV[1]),"\n\n")
    cat(" ",paste0(rownames(glm_L$Conf_Matrix_CV[2]$table),collapse = " "),"\n")
    cat("0",paste0(glm_L$Conf_Matrix_CV[2]$table[1,],collapse = " "),"\n")
    cat("1",paste0(glm_L$Conf_Matrix_CV[2]$table[2,],collapse = " "),"\n\n")
    cat(paste0(names(glm_L$Conf_Matrix_CV[3]$overall),collapse = ""),"\n")
    cat(glm_L$Conf_Matrix_CV[3]$overall,"\t\n\n")
    cat(paste0(names(glm_L$Conf_Matrix_CV[4]$byClass)),"\t\n",glm_L$Conf_Matrix_CV[4]$byClass,"\t\n\n")         
    cat("Area Under Curve Score for",input$ML_Method,":",glm_L$AUC)

    }  
    
  },outputArgs = list())
  
}

# Run the application 
shinyApp(ui = ui, server = server)

