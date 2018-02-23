library(shiny)
library(igraph)
setwd(getwd())
shinyUI(
  pageWithSidebar(
    headerPanel = (title = ""),
    
    sidebarPanel(
      
      selectInput("Correlation","Please select one correlation",
                  choices = c("Pearson","Spearman")),
      
      selectInput("Method","Please select one method",
                  choices = c("Asymptotic","Bootstrap")
        
      ),
      
      sliderInput("alpha","Please select the alpha value",
                  min=0.00001,max=1,value=0.05,
                  step=0.01),
      sliderInput("epsilon","Please select an epsilon value: ",min=0,max=1,value=0.3,
                  step=0.05)
      ),
     mainPanel( plotOutput("myPlot")
                
                )
)
    )
