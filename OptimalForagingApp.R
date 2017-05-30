# Load neccesary packages -----------------------------------------------------------

if(!"shiny" %in% installed.packages()) 
{ 
    install.packages("shiny") 
}
library(shiny)

if(!"shinyjs" %in% installed.packages()) 
{ 
    install.packages("shinyjs") 
}
library(shinyjs)

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

if(!"markdown" %in% installed.packages()) 
{ 
    install.packages("markdown") 
}
library(markdown)

knitr::knit("manual.Rmd")

# Define UI ---------------------------------------------------------------

ui <- navbarPage(title = "The Optimal Foraging app",
                 theme = shinytheme("united"),
                 
                 ## Tab 1 UI ################################################################
                 
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

                                    #participant_result {
                                    color: #ff0000
                                    }
                                   
                                   body, label, input, button, select { 
                                   font-family: "Arial";
                                   }')
                          )),
                          tags$a(
                              tags$img(style="position: absolute; top: 10; right: 0; border: 0; width: 500px; height: 100px",
                                       src="uva-acroniem.png")
                          ),
                          
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
                                  h3(textOutput(outputId = "participant_result")),
                                  fluidRow(
                                      splitLayout(cellWidths = c("15%","45%", "55%"), tableOutput(outputId = 'responses'),plotOutput("simitem"), plotOutput("similarity"))
                                  ),
                                  plotOutput(outputId = 'RT'),
                                  textOutput(outputId = 'model'),
                                  
                                  downloadButton('download_task', 'Download results')
                              )
                          )
                 ),
                 
                 # Tab 2 UI ################################################################
                 
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

                                   #link {
                                   color: #0066ff
                                   }

                                    #error {
                                    color: #ff0000
                                    }

                                    #final {
                                    color: #ff0000 
                                   }
                                   
                                   body, label, input, button, select { 
                                   font-family: "Arial";
                                   }')
                          )),
                          tags$a(
                              tags$img(style="position: absolute; top: 10; right: 0; border: 0; width: 500px; height: 100px",
                                       src="uva-acroniem.png")
                          ),
                          
                          headerPanel("The file upload"),
                          sidebarLayout(
                              sidebarPanel(id = "sidebar",
                                           useShinyjs(),
                                           checkboxInput(inputId = "show",label = "Show data requirements"),
                                           h4(id = "line0","The file should contain (variable names in brackets [ ]):"),
                                           p(id = "line1","Column 1 [sid]: ID of participant"),
                                           p(id = "line2","Column 2 [entry]: Response of participant"),
                                           p(id = "line3","Column 3 [irt]: Reaction time of participant"),
                                           p(id = "line4","Column 4 [fpatchnum]: Patch number of word"),
                                           p(id = "line5","Column 5 [fpatchitem]: Number of the word in the patch"),
                                           p(id = "line6","Column 6 [fitemsfromend]: From items from end"),
                                           p(id = "line7","Column 7 [flastitem]: Number removed from last item"),
                                           p(id = "line8","Column 8 [meanirt]: Mean reaction time of participant"),
                                           p(id = "line9","Column 9 [catitem]: Category of the word"),
                                           
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
                                           fileInput(inputId = "file",label = "Choose file",multiple = FALSE),
                                           
                                           h4(a(id = "link","Example dataset", href = "https://www.dropbox.com/s/37is6uf3jrhx2g0/testdata.csv?dl=0"))
                              ),
                              mainPanel(
                                  titlePanel("The results"),
                                  
                                  br(),
                                  textOutput(outputId = "error"),
                                  
                                  h3(textOutput(outputId = "final")),
                                  
                                  br(),

                                  fluidRow(                                
                                      splitLayout(cellWidths = c("50%", "50%"), plotOutput("itemplot"), plotOutput("simplot"))
                                  ),
                                  plotOutput(outputId = 'RTplot'),
                                  
                                  downloadButton('download_analysis', 'Download output')
                                 
                              )
                          )),

                 # Tab 3 UI ################################################################
                 
                 tabPanel(title = "Theory", 
                          
                          strong(id = "text", 
                                 span("Created by "),
                                 a("Koen Derks", href = "https://www.linkedin.com/in/koen-derks-283273124/"),
                                 HTML("&bull;"),
                                 span("Code"),
                                 a("on GitHub", href = "https://github.com/koenderks/OptimalForaging")
                          ),
                          tags$a(
                              tags$img(style="position: absolute; top: 10; right: 0; border: 0; width: 500px; height: 100px",
                                       src="uva-acroniem.png")
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
                 ),
                 
                 # Tab 4 UI ######################################################################
                 tabPanel(title = "Manual",
                          mainPanel(
                              includeMarkdown("manual.md")
                          )
                 )
)


# Server ------------------------------------------------------------------

server <- function(input, output) {
    
    ## Load Functions ################################################################
    
    source("tabOneFunctions.R")
    source("tabTwoFunctions.R")
    
    ## Load neccesary data ################################################################
    
    data <- read.csv("animal_clusters.csv",sep = ";",stringsAsFactors = FALSE)
    load("onlinedata.Rdata")
    
    ## Create placeholders Tab 1 ################################################################
    
    df <- data.frame()
    ancos <- ancos
    tab <- NULL
    sim <- NULL
    simitem <- NULL
    hoi <- NULL
    
    ## Create placeholders Tab 2 ################################################################
    
    blank <- data.frame()
    itemplot <- NULL
    simplot <- NULL
    RTplot <- NULL
    
    ## Create empty plots tab 1 ################################################################
    
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
            ylab("Time spent on word (s)")
    })
    
    ## Create empty plots Tab 2 ################################################################
    
    output$itemplot <- renderPlot({ggplot(blank) + 
            ylab("BEAGLE similariry") +
            xlab("Item's position preceding most recent item") +
            ggtitle("Mean similarity with previous word") +
            ylim(c(0,0.5)) +
            xlim(c(0,5))
    },width = 400,height = 400)
    
    output$simplot <- renderPlot({
        ggplot(blank,aes(seq_along(blank),blank))+
            geom_bar(stat="identity", fill = "magenta3") +
            ylab("Residual proximity") +
            xlab("Order of entry relative to patch switch") +
            ggtitle("Mean residual proximity for words") +
            ylim(c(0,0.7)) +
            xlim(c(0,5))
    },width = 400,height = 400)
    
    output$RTplot <- renderPlot({
        ggplot(blank,aes(seq_along(blank), blank)) + 
            geom_point(col = "turquoise3") +
            ylab("Number of words produced") +
            xlab("Absolute difference between mean last item IRT and mean overall IRT (sec)") +
            xlim(c(0,40)) +
            ylim(c(0,10))
    })

    # Create timer ####################################################################
    
    output$timer <- renderText("Time left: 60 secs")
    
    # Run program tab 1 ################################################################
    
    observeEvent(input$start,once = FALSE, {
        
        # reset if "submit" is clicked
        clicked <- 0
        
        tab <<- NULL
        sim <<- NULL
        simitem <<- NULL
        
        stoptime <- Sys.time() + 60
        
        # update the timer ####
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
        
        # save the responses
        mat <- matrix(nrow=0,ncol=3)
        colnames(mat)<- c("word", "switch_patch", "switch_cat")
        results <- data.frame(mat)
        
        responses_output <- matrix(nrow = 0,ncol = 2)
        colnames(responses_output) <- c("response", "RT")
        responses_output <- data.frame(responses_output)
        
        current <- Sys.time()
        
        iter <- 0
        
        index_vector <- rep(0,25)
        
        color_vector <- rep("black",20)
        
        # observe submit button
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
                
                # participant switched from patch?
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

        #time_plot <- NULL
        
        # observe timer
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
        
        output$participant_result <- renderText({
            invalidateLater(10000)
            proportion <- (length(which(color_vector=="indianred2"))-1)/length(which(color_vector != "black"))
            if(as.numeric(round(difftime(stoptime, Sys.time(), units='secs'))) < 0){
                if(proportion <= 0.33){
                    paste("You switched patches", length(which(color_vector=="indianred2"))-1, "out of", length(which(color_vector != "black")), "times and displayed an optimal foraging technique.")
                } else {
                    paste("You switched patches", length(which(color_vector=="indianred2"))-1, "out of", length(which(color_vector != "black")), "times and did not display an optimal foraging technique.")
                }
            }
        })
        
        time_plot <<- time_plot
        tab <<- tab
        
    })
    
    ## Download button tab 1 ################################################################
    
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
                
                simitem, # item plot
                
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
    
    ## Run program Tab 2 ################################################################
    
    observe({
        toggle("line0",anim = TRUE,condition = input$show)
        toggle("line1",anim = TRUE,condition = input$show)
        toggle("line2",anim = TRUE,condition = input$show)
        toggle("line3",anim = TRUE,condition = input$show)
        toggle("line4",anim = TRUE,condition = input$show)
        toggle("line5",anim = TRUE,condition = input$show)
        toggle("line6",anim = TRUE,condition = input$show)
        toggle("line7",anim = TRUE,condition = input$show)
        toggle("line8",anim = TRUE,condition = input$show)
        toggle("line9",anim = TRUE,condition = input$show)
    })
    
    observe({
        
        progress <- shiny::Progress$new()
        
        on.exit(progress$close())
        
        progress$set(message = "Running analysis", value = 0)
        
        progress$inc(0.10, detail = 'Uploading data')
        
        file <- input$file
        
        if(is.null(file)){
            return(NULL)
        } else if (input$type == "csv"){
            dat <- read.csv(file$datapath,header = input$header,sep = input$separator)
        } else if (input$type == "txt"){
            dat <- read.table(file$datapath, header = input$header, sep = input$separator)
        } else if (input$type == "xlsx"){
            dat <- read.xlsx(file$datapath, header = input$header, sep = input$separator,sheetIndex = 1)
        }
        
        baddata <- FALSE
        
        if(colnames(dat)[1] == "X"){
            if(all(colnames(dat)[2:10] != c("sid", "entry", "irt", "fpatchnum", "fpatchitem", "fitemsfromend", "flastitem", "meanirt", "catitem"))){
                output$error <- renderText({
                    invalidateLater(10000)
                    "Cannot perform analysis: data does not meet data requirements"})
                baddata <- TRUE
            }
        } else if(colnames(dat)[1] != "X"){
            if(all(colnames(dat) != c("sid", "entry", "irt", "fpatchnum", "fpatchitem", "fitemsfromend", "flastitem", "meanirt", "catitem"))){
                output$error <- renderText({
                    invalidateLater(10000)
                    "Cannot perform analysis: data does not meet data requirements"})
                baddata <- TRUE
            }
        }
        
        if(baddata == FALSE){
            
        output$error <- renderText("")
        
        progress$inc(0.10, detail = 'Starting analysis')
        
        # run analysis ##
        dccc <- dat
        dccc <- data.frame(dccc["sid"], dccc["entry"])
        names(dccc) <- c("sid", "entry")
        
        simback <- .computeSimilarity(dccc,ancos)
        
        simback1 <- simback[[1]]
        simback2 <- simback[[2]]
        simback3 <- simback[[3]]
        simback4 <- simback[[4]]
        simback5 <- simback[[5]]
        
        .testSimilarity(simback1,simback2,simback3,simback4,simback5)
        
        progress$inc(0.10, detail = 'Computing similarity')
        
        dddd <- data.frame(dccc, simback1, simback2, simback3, simback4, simback5)
        
        sb1 <- with(dddd, tapply(simback1, sid, mean, na.rm=T))
        sb2 <- with(dddd, tapply(simback2, sid, mean, na.rm=T))
        sb3 <- with(dddd, tapply(simback3, sid, mean, na.rm=T))
        sb4 <- with(dddd, tapply(simback4, sid, mean, na.rm=T))
        sb5 <- with(dddd, tapply(simback5, sid, mean, na.rm=T))
        
        x <- rep(1:5, each=141)
        
        a_table <- data.frame(x, sim = c(sb1, sb2, sb3, sb4, sb5))
        
        res <- anova(lm(sim~factor(x), data=a_table))
        
        if(res$`Pr(>F)`[1] < .05){
            output$final <- renderText("The optimal foraging model fits the data.")
        } else {
            output$final <- renderText("The optimal foraging model does not fit the data.")
        }
        
        ms <- c(mean(sb1, na.rm=T),
                mean(sb2, na.rm=T),
                mean(sb3, na.rm=T),
                mean(sb4, na.rm=T),
                mean(sb5, na.rm=T)
        )
        sdd <- c(sd(sb1, na.rm=T),
                 sd(sb2, na.rm=T),
                 sd(sb3, na.rm=T),
                 sd(sb4, na.rm=T),
                 sd(sb5, na.rm=T)
        )
        see <- sdd / sqrt(141)
        
        progress$inc(0.10, detail = 'Producing plot')
        
        output$itemplot <- renderPlot({
            ggplot(data.frame(ms[5:1]), aes(seq_along(ms[5:1]),ms[5:1])) + 
                geom_bar(stat = "identity", fill = "indianred2") +
                ylab("BEAGLE similarity") +
                xlab("Item's position preceding most recent item") +
                ggtitle("Mean similarity with previous word") +
                ylim(c(0,0.5)) + 
                geom_errorbar(aes(ymin = ms[5:1]-see[5:1], ymax = ms[5:1]+see[5:1],width = .3))
        })
        itemplot <<-  ggplot(data.frame(ms[5:1]), aes(seq_along(ms[5:1]),ms[5:1])) + 
            geom_bar(stat = "identity", fill = "indianred2") +
            ylab("BEAGLE similarity") +
            xlab("Item's position preceding most recent item") +
            ggtitle("Mean similarity with previous word") +
            ylim(c(0,0.5)) + 
            geom_errorbar(aes(ymin = ms[5:1]-see[5:1], ymax = ms[5:1]+see[5:1],width = .3))
        
        progress$inc(0.10, detail = 'Computing proximity')
        
        db <- dat
        rowsim <- rep(0, nrow(db))
        lastid <- 0
        for(i in 1:nrow(db)){
            if (lastid != toString(db$sid[i])){
                lastid <- db$sid[i]
                subcos <- ancos
            }
            subsub <- subcos[toString(db$entry[i]),]
            subsub <- subsub[subsub < 1]
            rowsim[i] <- mean(subsub)
            if( toString(db$entry[i] %in% colnames(subcos))){
                x <- 1:ncol(subcos)
                y <- x[toString(db$entry[i]) == colnames(subcos)]
                subcos <- subcos[,-y]
            }	
        }
        
        dtest <- data.frame(entrydata, rowsim)
        db <- dtest
        
        hits <- matrix(999, nrow(db), ncol = 8)
        
        for( i in 1:(nrow(db))){
            if(db$fpatchnum[i] > 0){
                if (db$fpatchitem[i] < 5) hits[i,4+db$fpatchitem[i]] <- db$rowsim[i]
                if (db$fitemsfromend[i] < 5 && db$fpatchitem[i] != 1) hits[i,5-db$fitemsfromend[i]] <- db$rowsim[i]
            }
        }
        
        progress$inc(0.20, detail = 'Computing residuals')
        
        msss <- c(
            mean(hits[hits[,1] < 999, 1], na.rm = T),
            mean(hits[hits[,2] < 999, 2], na.rm = T),
            mean(hits[hits[,3] < 999, 3], na.rm = T),
            mean(hits[hits[,4] < 999, 4], na.rm = T),
            mean(hits[hits[,5] < 999,5], na.rm = T),
            mean(hits[hits[,6] < 999, 6], na.rm = T),
            mean(hits[hits[,7] < 999, 7], na.rm = T),
            mean(hits[hits[,8] < 999, 8], na.rm = T))
        
        msse <- c(
            sd(hits[hits[,1] < 999,1], na.rm = T) / sqrt(length(hits[hits[,1] < 999,1])-sum(as.numeric(is.na(hits[,1])))),
            sd(hits[hits[,2] < 999,2], na.rm = T) / sqrt(length(hits[hits[,2] < 999,2])-sum(as.numeric(is.na(hits[,2])))),
            sd(hits[hits[,3] < 999,3], na.rm = T) / sqrt(length(hits[hits[,3] < 999,3])-sum(as.numeric(is.na(hits[,3])))),
            sd(hits[hits[,4] < 999,4], na.rm = T) / sqrt(length(hits[hits[,4] < 999,4])-sum(as.numeric(is.na(hits[,4])))),
            sd(hits[hits[,5] < 999,5], na.rm = T) / sqrt(length(hits[hits[,5] < 999,5])-sum(as.numeric(is.na(hits[,5])))),
            sd(hits[hits[,6] < 999,6], na.rm = T) / sqrt(length(hits[hits[,6] < 999,6])-sum(as.numeric(is.na(hits[,6])))),
            sd(hits[hits[,7] < 999,7], na.rm = T) / sqrt(length(hits[hits[,7] < 999,7])-sum(as.numeric(is.na(hits[,7])))),
            sd(hits[hits[,8] < 999,8], na.rm = T) / sqrt(length(hits[hits[,8] < 999,8])-sum(as.numeric(is.na(hits[,8])))))
        
        .testProximity(msss)
       
         progress$inc(0.10, detail = 'Creating plot')
        
        output$simplot <- renderPlot({
            ggplot(data.frame(msss[c(3,4,5,6,7)]),aes(seq_along(msss[c(3,4,5,6,7)]),msss[c(3,4,5,6,7)])) +
                geom_bar(stat = "identity", fill = "turquoise3") +
                xlab("Order of entry relative to patch switch") +
                ylab("Residual Proximity") +
                ggtitle("Mean residual proximity for words") +
                geom_errorbar(ymin = msss[c(3,4,5,6,7)]-msse[c(3,4,5,6,7)], ymax = msss[c(3,4,5,6,7)]+msse[c(3,4,5,6,7)],width = .3)
        })
        simplot <<- ggplot(data.frame(msss[c(3,4,5,6,7)]),aes(seq_along(msss[c(3,4,5,6,7)]),msss[c(3,4,5,6,7)])) +
            geom_bar(stat = "identity", fill = "turquoise3") +
            xlab("Order of entry relative to patch switch") +
            ylab("Residual Proximity") +
            ggtitle("Mean residual proximity for words") +
            geom_errorbar(ymin = msss[c(3,4,5,6,7)]-msse[c(3,4,5,6,7)], ymax = msss[c(3,4,5,6,7)]+msse[c(3,4,5,6,7)],width = .3)
        
        progress$inc(0.10, detail = 'Analyzing reaction times')
        
        dccc <- dat
        subs <- unique(dccc$sid)
        meanswi.irt <- rep(0, length(subs))
        meanoverall.irt <- rep(0, length(subs))
        prods <- rep(0, length(subs))
        
        progress$inc(0.10, detail = 'Finishing analysis')
        
        for (i in 1:length(subs)){
            datsub <- subset(dccc, sid == subs[i])
            datsubflast <- subset(datsub, flastitem == 1)
            meanswi.irt[i] <- mean(datsubflast$irt)
            meanoverall.irt[i] <- sum(datsub$irt)/nrow(datsub)
            prods[i] <- nrow(datsub)
        }
        
        .testReactionTime(meanswi.irt,meanoverall.irt)
        
        output$RTplot <- renderPlot({
            ggplot(data.frame(abs(meanswi.irt-meanoverall.irt)),aes(abs(meanswi.irt-meanoverall.irt), prods)) + 
            geom_point(col = "turquoise3") +
            ylab("Number of words produced") +
            xlab("Absolute difference between mean last item IRT and mean overall IRT (sec)") +
            geom_smooth(method='lm', col = "indianred2")
        })
        RTplot <<- ggplot(data.frame(abs(meanswi.irt-meanoverall.irt)),aes(abs(meanswi.irt-meanoverall.irt), prods)) + 
            geom_point(col = "turquoise3") +
            ylab("Number of words produced") +
            xlab("Absolute difference between mean last item IRT and mean overall IRT (sec)") +
            geom_smooth(method='lm', col = "indianred2")
        
        }
        
    })
    
    ## Download button Tab 2 ############################################################
    
    output$download_analysis <- downloadHandler(
        
        filename = function()
        {
            paste("OptimalForagingAnalysis", class = ".pdf", sep = "")
        },
        
        content = function(file) 
        {
            pdf(file,paper = "a4")
            
            grid.arrange(
                
                itemplot,
                
                simplot,
                
                RTplot
                
            )
            
            dev.off()
        })
    
}

# Run the application 
shinyApp(ui = ui, server = server)

