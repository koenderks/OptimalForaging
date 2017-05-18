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

