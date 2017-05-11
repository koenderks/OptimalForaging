############################
# Load neccesary packages ##
############################

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

if(!"gridExtra" %in% installed.packages()) 
{ 
    install.packages("gridExtra") 
}
library(gridExtra)

#######
# UI ##
#######

ui <- navbarPage(title = "Optimal Foraging modeling",
                 theme = shinytheme("united"),
                 
                 ###########
                 ## Tab 1 ##
                 ###########
                 
                 tabPanel(title = "Task",
                          
                          strong(id = "text", 
                                 span("Created by "),
                                 a("Koen Derks", href = "https://www.linkedin.com/in/koen-derks-283273124/"),
                                 HTML("&bull;"),
                                 span("Code"),
                                 a("on GitHub", href = "https://github.com/koenderks/OptimalForaging")
                          ),
                          
                          tags$head(tags$style(
                              HTML('
                                   #sidebar {
                                   background-color: #FABC3C;
                                   }

                                    #submit {
                                    background-color: #9C3848
                                    }
                                    #start {
                                    background-color: #9C3848
                                    }
                                    #download_task {
                                    background-color: #9C3848
                                   }
                                   
                                   body, label, input, button, select { 
                                   font-family: "Arial";
                                   }')
                          )),
                          
                          # Title panel
                          headerPanel("The task"),
                          # sidebar
                          sidebarLayout(
                              sidebarPanel(id="sidebar",
                                           tags$head(tags$script(src = "enter-button.js")),
                                           h3("Introduction"),
                                           h4(p("The purpose of this task is to name as much animals as possible within the time limit of 60 seconds.")),
                                           br(),
                                           actionButton("start", "Start the timer",icon = icon("clock-o")),
                                           br(),
                                           h3(textOutput(outputId = "timer")),
                                           br(),
                                           textInput(inputId = "word",label = "Name an animal and press enter (or submit):",placeholder = "Name an animal:"),
                                           actionButton(inputId = "submit", label = "Submit", icon = icon("check"))
                              ),
                              mainPanel(
                                  titlePanel("The responses"),
                                  fluidRow(
                                      splitLayout(cellWidths = c("15%","45%", "55%"), tableOutput(outputId = 'responses'),plotOutput("simitem"), plotOutput("similarity"))
                                  ),
                                  plotOutput(outputId = 'RT'),
                                  textOutput(outputId = 'model'),
                                  
                                  downloadButton('download_task', 'Download results')
                              )
                          )
                 ),
                 
                 ##########
                 # Tab 2 ##
                 ##########
                 
                 tabPanel("Upload file",
                          
                          strong(id = "text", 
                                 span("Created by "),
                                 a("Koen Derks", href = "https://www.linkedin.com/in/koen-derks-283273124/"),
                                 HTML("&bull;"),
                                 span("Code"),
                                 a("on GitHub", href = "https://github.com/koenderks/OptimalForaging")
                          ),
                          
                          tags$head(tags$style(
                              HTML('
                                   #sidebar {
                                   background-color: #FABC3C;
                                   }

                                   #download_analysis {
                                   background-color: #9C3848
                                   }
                                   
                                   body, label, input, button, select { 
                                   font-family: "Arial";
                                   }')
                          )),
                          
                          headerPanel("The file upload"),
                          sidebarLayout(
                              sidebarPanel(id = "sidebar",
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
                 
                 ##########
                 # Tab 3 ##
                 ##########
                 
                 tabPanel(title = "Theory", 
                          
                          strong(id = "text", 
                                 span("Created by "),
                                 a("Koen Derks", href = "https://www.linkedin.com/in/koen-derks-283273124/"),
                                 HTML("&bull;"),
                                 span("Code"),
                                 a("on GitHub", href = "https://github.com/koenderks/OptimalForaging")
                          ),
                          
                          headerPanel("The theory"),
                          br(),
                          h3("The relation with the animal kingdom"),
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
                            long-term average rate achieved by following the optimal strategy."),
                          img(src = "bee.jpg", height = 300, width = 300,style="display: block; margin-left: auto; margin-right: auto;"),
                          h3("The math"),
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
                          h4(withMathJax("$$R=\\frac{g(t_W)}{t_W + t_B}$$")),
                          p("where tW is the time spent foraging within each resource patch, tB
                    is the average time spent traveling between patches, and g(tW) is
                    the cumulative gain within a patch.
                    Equation 1 provides a measure of resources per time unit, as a
                    function of an individual's control over their time tW within a
                    patch. This is subject to patch quality, reflected by g(tW), and travel
                    time tB between patches. The organism is predicted to spend the
                    optimal amount of time in a patch (t*) such that R is maximized:"),
                          h4(withMathJax("$$R^* = g'(t^*)$$")),
                          p("To maximize this resource intake, the optimal foraging policy is to
                            leave a patch at time t*, when the instantaneous rate (or marginal
                            value) of resource gain, g(t*), is equal to the long-term average
                            resource intake over the entire environment (patches and time
                            between), R*. In other words, the organism will switch to betweenpatch
                            search when the within-patch rate (which usually starts high
                            in a new, undepleted patch) drops to R*. The foundational assumption of the
                            model is that recall is achieved by probing retrieval structures in
                            memory with a specific cue set, that is, the memory probe. With I
                            representing a possible target item for recovery in the search space,
                            the probability of retrieving I is computed as the product of the
                            individual retrieval strengths for I across a probe set of M cues,
                            with S(Q, I) representing the semantic similarity between cue Q
                            and item I. This is incorporated into an overall probability of
                            retrieval for item I via the ratio rule:"),
                          h4(withMathJax("$$P(I_i|Q_1,Q_2,...,Q_M) = \\frac{\\prod_{j=1}^M S(Q_j,I_i)^{B_j}}{\\sum_{k=1}^N \\prod_{j=1}^M S(Q_j,I_k)^{B_j}}$$")),
                          p("where N represents the total number of items available in the
                            category for retrieval and B represents the saliency (or attention
                            weight) assigned to a given cue."),
                          img(src = "Optimal_Foraging_Theory.jpg", height = 400, width = 400,style="display: block; margin-left: auto; margin-right: auto;")
                 )
)

###########
# Server ##
###########

server <- function(input, output) {
    
    #####################
    # define functions ##
    #####################
    
    # Tab 1
    
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
            
            ggplot(data.frame(rev(index_vector)),aes(seq_along(index_vector),index_vector))+
                geom_bar(stat="identity", fill = color_vector) +
                ylab("BEAGLE similariry") +
                xlab("Word index") +
                ggtitle("Similarity with previous word",subtitle = "red indicates a patch swith")+
                xlim(c(0,20)) +
                ylim(c(0,0.7))
            
        } else {
            
            if(results[nrow(results),2]=="FALSE"){
                
                ggplot(data.frame(rev(index_vector)),aes(seq_along(index_vector),index_vector))+
                    geom_bar(stat="identity", fill = color_vector) +
                    ylab("BEAGLE similariry") +
                    xlab("Word index") +
                    ggtitle("Similarity with previous word",subtitle = "red indicates a patch swith") +
                    xlim(c(0,20)) +
                    ylim(c(0,0.7))  
                
            } else if(results[nrow(results),2]=="TRUE"){
                
                ggplot(data.frame(rev(index_vector)),aes(seq_along(index_vector),index_vector))+
                    geom_bar(stat="identity", fill = color_vector) +
                    ylab("BEAGLE ") +
                    xlab("Word index") +
                    ggtitle("Similarity with previous word",subtitle = "red indicates a patch swith") +
                    xlim(c(0,20)) +
                    ylim(c(0,0.7))
                
            }
        }
        
    }
    
    .RTplot2 <- function(time){
        
        ggplot(data.frame(time), aes(seq_along(time),time)) +
            ggtitle("Reaction time") + 
            geom_line(size = 2, col = "turquoise3", linetype = 1) +
            xlab("Time (0.3 s)") +
            ylab("Time spent on item") +
            geom_hline(yintercept = mean(time), col = "indianred2",linetype = 2,size = 1.5)
        
    }
    
    # Tab 2 ##
    
    ########################
    # Load neccesary data ##
    ########################
    
    data <- read.csv("animal_clusters.csv",sep = ";",stringsAsFactors = FALSE)
    load("onlinedata.Rdata")
    
    ########################
    # Create placeholders ##
    ########################
    
    # Tab 1 ##
    
    df <- data.frame()
    ancos <- ancos
    tab <- NULL
    sim <- NULL
    simitem <- NULL
    
    # Tab 2 ##
    
    ##########################################################
    # Create empty plots to display if nothing happened yet ##
    ##########################################################
    
    # Tab 1 ##
    
    output$simitem <- renderPlot({ggplot(df) + 
            ylab("BEAGLE similariry") +
            xlab("Word index") +
            ggtitle("Similarity with previous word",subtitle = "red indicates a patch swith") +
            xlim(c(0,20)) +
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
    output$RT <- renderPlot({
        ggplot(data.frame()) +
            xlim(c(0,200)) +
            ylim(c(0,5)) +
            ggtitle("Reaction time") +
            xlab("Time (0.3 s)") +
            ylab("Time spent on item")
    })
    
    # Tab 2 ##
    
    #################
    # Create timer ##
    #################
    
    output$timer <- renderText("Time left: 60 secs")
    
    ################
    # Run program ##
    ################
    
    # Tab 1 ## 
    
    # TODO: fit optimal foraging model on data from participant.
    
    observeEvent(input$start,once = FALSE, {
        
        clicked <- 0
        # create for plot
        tab <<- NULL
        sim <<- NULL
        simitem <<- NULL
        
        stoptime <- Sys.time() + 60
        
        output$timer <- renderText({
            invalidateLater(10)
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
        
        index_vector <- rep(0,25)
        
        color_vector <- rep("turquoise3",20)
        
        observeEvent(input$submit, {
            
            clicked <<- 1
            
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
                
                RT <- as.numeric(round(difftime(Sys.time(),current, units='secs')))
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
                        # save for plot
                        sim <<- similarity
                        
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
                simitem <<- .updateBarPlot(index_vector, iter, results,color_vector, valid)
                
            } 
            
            
            output$responses <- renderTable(responses_output)
            
            tab <<- responses_output
            
        })
        
        time <- 0
        
        observe({
            
            invalidateLater(millis = 0.1)
            
            if(clicked == 0 && as.numeric(round(difftime(stoptime, Sys.time(), units='secs'))) >= 0){
                
                time <<- c(time, time[length(time)] + 0.60)
                
            } else if(clicked == 1 && as.numeric(round(difftime(stoptime, Sys.time(), units='secs'))) >= 0){
                
                time <<- c(time, time[length(time)] - time[length(time)])
                
            }
            
            clicked <<- 0
            
            output$RT <- renderPlot({.RTplot2(time)})
            
            time_plot <<- time
            
        })
        
        # save for plot
        time_plot <<- time_plot
        tab <<- tab
        
    })
    
    output$download_task <- downloadHandler(
        
        filename = function()
        {
            paste("OptimalForagingTask", class = ".pdf", sep = "")
        },
        
        content = function(file) 
        {
            pdf(file,paper = "a4")
            
            if(!is.null(tab)){
                grid.table(tab)
            }
            
            grid.arrange(
                
                simitem, # time plot
                
                ggplot(data.frame(sim),aes(seq_along(sim),sim))+
                    geom_bar(stat="identity", fill = "magenta3") +
                    ylab("BEAGLE simlilarity") +
                    xlab("Item's position preceding most recent item") +
                    ggtitle("Similarity with previous 5 words") +
                    ylim(c(0,0.7)), 
                
                ggplot(data.frame(time_plot), aes(seq_along(time_plot),time_plot)) +
                    ggtitle("Reaction time") + 
                    geom_line(size = 2, col = "turquoise3", linetype = 1) +
                    xlab("Time (0.3 s)") +
                    ylab("Time spent on item") +
                    geom_hline(yintercept = mean(time_plot), col = "indianred2",linetype = 3,size = 1.5)
                
            ) # end grid arrange
            dev.off()
        })
    
    # Tab 2 ##
    
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

