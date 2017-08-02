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
            ylab("Similariry") +
            xlab("Word index") +
            ggtitle("Similarity with previous word",subtitle = "red indicates a patch swith")+
            xlim(c(0,20)) +
            ylim(c(0,0.7)) +
            theme(axis.line = element_line(colour = "black"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank())
        
    } else {
        
        if(results[nrow(results),2]=="FALSE"){
            
            ggplot(data.frame(rev(index_vector)),aes(seq_along(index_vector),index_vector))+
                geom_bar(stat="identity", fill = color_vector) +
                ylab("Similariry") +
                xlab("Word index") +
                ggtitle("Similarity with previous word",subtitle = "red indicates a patch swith") +
                xlim(c(0,20)) +
                ylim(c(0,0.7)) +
                theme(axis.line = element_line(colour = "black"),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.border = element_blank(),
                      panel.background = element_blank())  
            
        } else if(results[nrow(results),2]=="TRUE"){
            
            ggplot(data.frame(rev(index_vector)),aes(seq_along(index_vector),index_vector))+
                geom_bar(stat="identity", fill = color_vector) +
                ylab("Similarity") +
                xlab("Word index") +
                ggtitle("Similarity with previous word",subtitle = "red indicates a patch swith") +
                xlim(c(0,20)) +
                ylim(c(0,0.7)) +
                theme(axis.line = element_line(colour = "black"),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.border = element_blank(),
                      panel.background = element_blank())
            
        }
    }
    
}

.updateBarPlotNL <- function(index_vector,iter, results, color_vector,valid){
    
    if(iter == 1){
        
        ggplot(data.frame(rev(index_vector)),aes(seq_along(index_vector),index_vector))+
            geom_bar(stat="identity", fill = color_vector) +
            ylab("Gelijkenis") +
            xlab("Woord index") +
            ggtitle("Gelijkenis met vorige woord",subtitle = "Rode kleur is een indicator van patch wissel")+
            xlim(c(0,20)) +
            ylim(c(0,0.7)) +
            theme(axis.line = element_line(colour = "black"),
                  panel.grid.major = element_blank(),
                  panel.grid.minor = element_blank(),
                  panel.border = element_blank(),
                  panel.background = element_blank())
        
    } else {
        
        if(results[nrow(results),2]=="FALSE"){
            
            ggplot(data.frame(rev(index_vector)),aes(seq_along(index_vector),index_vector))+
                geom_bar(stat="identity", fill = color_vector) +
                ylab("Gelijkenis") +
                xlab("Woord index") +
                ggtitle("Gelijkenis met vorige woord",subtitle = "Rode kleur is een indicator van patch wissel") +
                xlim(c(0,20)) +
                ylim(c(0,0.7)) +
                theme(axis.line = element_line(colour = "black"),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.border = element_blank(),
                      panel.background = element_blank())
            
        } else if(results[nrow(results),2]=="TRUE"){
            
            ggplot(data.frame(rev(index_vector)),aes(seq_along(index_vector),index_vector))+
                geom_bar(stat="identity", fill = color_vector) +
                ylab("Gelijkenis") +
                xlab("Woord index") +
                ggtitle("Gelijkenis met vorige woord",subtitle = "Rode kleur is een indicator van patch wissel") +
                xlim(c(0,20)) +
                ylim(c(0,0.7)) +
                theme(axis.line = element_line(colour = "black"),
                      panel.grid.major = element_blank(),
                      panel.grid.minor = element_blank(),
                      panel.border = element_blank(),
                      panel.background = element_blank())
            
        }
    }
    
}

.RTplot2 <- function(time){
    
    ggplot(data.frame(time), aes(seq_along(time),time)) +
        ggtitle("Reaction time") + 
        geom_line(size = 1.5, col = "black", linetype = 1) +
        xlab("") +
        ylab("Time spent on item (s)") +
        geom_hline(yintercept = mean(time), col = "red",linetype = 2,size = 1) +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())
    
}

.RTplot2NL <- function(time){
    
    ggplot(data.frame(time), aes(seq_along(time),time)) +
        ggtitle("Reactietijd") + 
        geom_line(size = 1.5, col = "black", linetype = 1) +
        xlab("") +
        ylab("Tijd besteed aan item (s)") +
        geom_hline(yintercept = mean(time), col = "red",linetype = 2,size = 1) +
        theme(axis.line = element_line(colour = "black"),
              panel.grid.major = element_blank(),
              panel.grid.minor = element_blank(),
              panel.border = element_blank(),
              panel.background = element_blank(),
              axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())
    
}

