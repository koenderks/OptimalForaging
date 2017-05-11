if(!"shiny" %in% installed.packages()) 
{ 
    install.packages("shiny") 
}
library(shiny)

if(!"ggplot2" %in% installed.packages()) 
{ 
    install.packages("ggplot2") 
}
library(ggplot2)

if(!"xlsx" %in% installed.packages()) 
{ 
    install.packages("xlsx") 
}
library(xlsx)

if(!"shinythemes" %in% installed.packages()) 
{ 
    install.packages("shinythemes") 
}
library(shinythemes)

# Define UI for application that draws a histogram
ui <- navbarPage("Optimal Foraging modeling",theme = shinytheme("united"),
                 
                 # tab 1
                 tabPanel("Task",
                          headerPanel("The task"),
                          sidebarLayout(
                              sidebarPanel(
                                  h3("Introduction"),
                                  p(strong("The purpose of this task is to name as much animals as possible within the time limit of 60 seconds.")),
                                  br(),
                                  actionButton("start", "Start the timer",icon = icon("clock-o")),
                                  br(),
                                  textOutput(outputId = "timer"),
                                  br(),
                                  textInput(inputId = "word",label = "Name an animal:"),
                                  actionButton(inputId = "submit", label = "Submit", icon = icon("check"))
                              ),
                              mainPanel(
                                  titlePanel("The responses"),
                                  fluidRow(
                                      splitLayout(cellWidths = c("15%","45%", "55%"), tableOutput(outputId = 'responses'),plotOutput("simitem"), plotOutput("similarity"))
                                  ),
                                  plotOutput(outputId = 'proximity'),
                                  plotOutput(outputId = 'RT'),
                                  
                                  downloadButton('download_task', 'Download output')
                              )
                          )
                 ),
                 
                 # tab 2
                 tabPanel("Upload file",
                          headerPanel("The file upload"),
                          sidebarLayout(
                              sidebarPanel(
                                  radioButtons(inputId = "type", label = "File Type:",
                                               choices = c(".csv" = "csv",
                                                           ".txt" = "txt",
                                                           ".xlsx" = "xlsx"),
                                               selected = "csv"),
                                  radioButtons(inputId = "separator",label = "Separator",
                                               choices = c("comma" = ",",
                                                           "semicolon" = ";",
                                                           "tab" = "\t"),
                                               selected = ","),
                                  checkboxInput(inputId = "header",label = "Header",value = TRUE),
                                  fileInput(inputId = "file",label = "Choose file",multiple = FALSE)
                                  # radioButtons(inputId = "quote", label = "Quote",
                                  #              choices = c("none" = "none",
                                  #                          "Double quote" = "double",
                                  #                          "Single quote" = "single")
                                  #              ,selected = "double")
                              ),
                              mainPanel(
                                  titlePanel("The results"),
                                  
                                  tableOutput(outputId = "test"),
                                  plotOutput(outputId = 'plot1'),
                                  plotOutput(outputId = 'plot2'),
                                  plotOutput(outputId = 'plot3'),
                                  
                                  downloadButton('download_analysis', 'Download output')
                              )
                          )),
                 
                 # tab 3
                 tabPanel("Theory",
                          headerPanel("The theory"),
                          br(),
                          h3("Relation with the animal kingdom"),
                          # paragraph
                          p("Animals often search for resources that occur in spatial patches,
                            such as the berries on separate bushes or nuts beneath a cluster of
                            trees. Humans also search for cognitive resources that can be seen as
                            patchy with respect to some metric other than space, such as memory
                            representations of words grouped by semantic categories (Bousfield
                            & Sedgewick, 1944; Raaijmakers & Shiffrin, 1981; Romney, Brewer,
                            & Batchelder, 1993), or sets of solutions that can be navigated by
                            working memory processes in a problem-solving task (Hills, Todd, &
                            Goldstone, 2008; Payne, Duggan, & Neth, 2007; Wilke, Hutchinson,
                            Todd, & Czienskowski, 2009)."),
                          p("In spatial environments, adaptive
                            foraging involves making appropriate global transitions between locally
                            exploited resource clusters: decisions that prevent animals from
                            staying too long in overexploited patches and from giving up too early
                            on patches full of resources yet to be found (Stephens & Krebs, 1987).
                            A classic model of optimal foraging theory (Charnov, 1976) predicts
                            that the overall rate of return is optimized if the forager leaves a patch
                            when the rate of finding new targets within the patch falls below the
                            long-term average rate achieved by following the optimal strategy.")),
                 img(src = "bee.jpg", height = 300, width = 300,style="display: block; margin-left: auto; margin-right: auto;"),
                 p("In the animal foraging literature, dynamic responses to the
                    environment are often assessed with respect to an optimal model
                   representing a hypothesis about the trade-offs that must be negotiated
                   in a given behavior- environment relationship. One of the
                   first and most successful models of optimal patch foraging at this
                   level is the marginal value theorem (Charnov, 1976). The marginal
                   value theorem assumes that resources are distributed in patches
                   that are monotonically depleted during foraging. The animal seeks
                   to maximize the gain per unit time of foraging defined as the
                   average resource intake, R, over all patches:"),
                 
                 # change this formula !
                 h2(withMathJax("$$X_n=X_{n-1}-\\varepsilon$$"))
                 #img(src = "Optimal_Foraging_Theory.jpg", height = 400, width = 400,style="display: block; margin-left: auto; margin-right: auto;")
                 )

server <- function(input, output) {
    
    # define functions
    
    .isValidInput <- function(results,word,indexes,data){
        
        valid <- TRUE
        not_unique <- FALSE
        
        if(length(which(results[,1]==word))!=0){
            not_unique <- TRUE
        }
        
        if(length(indexes)==0 | not_unique){
            
            indexes <- NULL
            valid <- FALSE
            patches <- NULL
            categories <- NULL
            
        } else {
            
            patches <- data[indexes,2]
            categories <- data[indexes,3]
            
        } 
        
        return(list(valid = valid, not_unique = not_unique, patches = patches, categories = categories))
        
    }
    
    .updateBarPlot <- function(index_vector,iter, results, color_vector,valid){
        
        if(iter == 1){
            
            ggplot(data.frame(index_vector),aes(seq_along(index_vector),index_vector))+
                geom_bar(stat="identity", fill = color_vector) +
                ylab("BEAGLE similariry") +
                xlab("Word index") +
                ggtitle("Similarity with previous word") +
                xlim(c(0,15)) +
                ylim(c(0,0.7))
            
        } else {
            
            if(results[nrow(results),2]=="FALSE"){
                
                ggplot(data.frame(index_vector),aes(seq_along(index_vector),index_vector))+
                    geom_bar(stat="identity", fill = color_vector) +
                    ylab("BEAGLE similariry") +
                    xlab("Word index") +
                    ggtitle("Similarity with previous word") +
                    xlim(c(0,15)) +
                    ylim(c(0,0.7))  
                
            } else if(results[nrow(results),2]=="TRUE"){
                
                ggplot(data.frame(index_vector),aes(seq_along(index_vector),index_vector))+
                    geom_bar(stat="identity", fill = color_vector) +
                    ylab("BEAGLE ") +
                    xlab("Word index") +
                    ggtitle("Similarity with previous word") +
                    xlim(c(0,15)) +
                    ylim(c(0,0.7)) 
                
            }
        }
        
    }
    
    data <- read.csv("animal_clusters.csv",sep = ";",stringsAsFactors = FALSE)
    load("onlinedata.Rdata")
    
    df <- data.frame()
    
    output$simitem <- renderPlot({ggplot(df) + 
            ylab("BEAGLE similariry") +
            xlab("Word index") +
            ggtitle("Similarity with previous word") +
            xlim(c(0,15)) +
            ylim(c(0,0.7))
    },width = 400,height = 400)
    output$similarity <- renderPlot({
        ggplot(df,aes(seq_along(df),df))+
            geom_bar(stat="identity", fill = "magenta3") +
            ylab("BEAGLE simlilarity") +
            xlab("Item's position preceding most recent item") +
            ggtitle("Similarity with previous 5 words") +
            ylim(c(0,0.7)) +
            xlim(c(0,5))
    },width = 400,height = 400)
    
    ancos <- ancos
    
    output$timer <- renderText("Time left: 60 secs")
    
    # tab 1
    
    observeEvent(input$start,once = FALSE, {
        
        stoptime <- Sys.time() + 60
        
        output$timer <- renderText({
            invalidateLater(1000)
            if(as.numeric(round(difftime(stoptime, Sys.time(), units='secs')))<0){
                paste("Time left:", 
                      0, 'secs')
            } else {
                paste("Time left:", 
                      round(difftime(stoptime, Sys.time(), units='secs')), 'secs')
            }
        })
        
        patches <- NULL
        categories <- NULL
        
        mat <- matrix(nrow=0,ncol=3)
        colnames(mat)<- c("word", "switch_patch", "switch_cat")
        results <- data.frame(mat)
        
        responses_output <- matrix(nrow = 0,ncol = 2)
        colnames(responses_output) <- c("response", "RT")
        responses_output <- data.frame(responses_output)
        
        current <- Sys.time()
        
        iter <- 0
        
        index_vector <- rep(0,20)
        
        color_vector <- rep("turquoise3",15)
        
        observeEvent(input$submit, {
            
            if(as.numeric(round(difftime(stoptime, Sys.time(), units='secs'))) >= 0){
                
                iter <<- iter + 1
                
                word <- input$word
                
                prev_patch <- patches
                prev_cat <- categories
                
                indexes <- which(data[,1]==word)
                
                # is the input valid?
                validinput <- .isValidInput(results = results,word = word,indexes = indexes,data=data)
                
                valid <- validinput[["valid"]]
                not_unique <- validinput[["not_unique"]]
                patches <<- validinput[["patches"]]
                categories <<- validinput[["categories"]]
                
                RT <- as.numeric(round(difftime(Sys.time(),current, units='auto')))
                current <<- Sys.time()
                
                tmp_response <- data.frame(word = word, RT = RT)
                responses_output <<- rbind(responses_output,tmp_response)
                
                # participant switched from patch or categorie?
                switch_patch <- !any(prev_patch == patches)
                switch_cat <- !any(prev_cat == categories)
                
                if(valid == TRUE){
                    
                    results[nrow(results)+1,] <<- c(word, switch_patch, switch_cat)
                    
                    if(iter > 5){
                        
                        similarity1 <- ancos[word,results[nrow(results)-1,1]]
                        similarity2 <- ancos[word,results[nrow(results)-2,1]]
                        similarity3 <- ancos[word,results[nrow(results)-3,1]]
                        similarity4 <- ancos[word,results[nrow(results)-4,1]]
                        similarity5 <- ancos[word,results[nrow(results)-5,1]]
                        
                        similarity <- c(similarity5,similarity4,similarity3,similarity2,similarity1)
                        
                        output$similarity <- renderPlot({
                            ggplot(data.frame(similarity),aes(seq_along(similarity),similarity))+
                                geom_bar(stat="identity", fill = "magenta3") +
                                ylab("BEAGLE simlilarity") +
                                xlab("Item's position preceding most recent item") +
                                ggtitle("Similarity with previous 5 words") +
                                ylim(c(0,0.7)) 
                        },width = 400,height = 400)
                        
                    }
                    
                }
                
                sim_last <- ancos[word,results[nrow(results)-1,1]]
                
                if(results[nrow(results),2] == "FALSE" & valid){
                    color_vector[nrow(results)] <<- "turquoise3"
                } else if (results[nrow(results),2] == "TRUE" & valid){
                    color_vector[nrow(results)] <<- "indianred2"
                }
                
                if(iter != 1 & !not_unique & valid){
                    index_vector[nrow(results)] <<- sim_last
                } 
                
                output$simitem <- renderPlot({.updateBarPlot(index_vector, iter, results,color_vector, valid)},width = 400,height = 400)
                
            } 
            
            output$responses <- renderTable(responses_output)
            
        })
        
    }) # end tab 1
    
    # tab 2
    
    output$test <- renderTable({
        
        file <- input$file
        
        if(is.null(file)){
            return(NULL)
        } else if (input$type == "csv"){
            read.csv(file$datapath,header = input$header,sep = input$separator)
        } else if (input$type == "txt"){
            read.table(file$datapath, header = input$header, sep = input$separator)
        } else if (input$type == "xlsx"){
            read.xlsx(file$datapath, header = input$header, sep = input$separator,sheetIndex = 1)
        }
        
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

