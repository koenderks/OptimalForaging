.testSimilarity <- function(simback1,simback2,simback3,simback4,simback5){
    if(any(length(simback1) == 0, length(simback2) == 0, length(simback3) == 0, length(simback4) == 0, length(simback5) == 0)){
        print("FAILED similarity test")
    } else {
        print("PASSED similarity test")
    }
}

.testProximity <- function(msss){
    if(is.null(msss)){
        print("FAILED proximity test")
    } else {
        print("PASSED proximity test")
    }
    
}

.testReactionTime <- function(meanswi.irt,meanoverall.irt){
    if(any(is.null(meanswi.irt),is.null(meanoverall.irt))){
        print("FAILED reaction time test")
    } else {
        print("PASSED reaction time test")
    }
}

.computeSimilarity <- function(dccc,ancos){
    
    simback1 <- rep(NA, nrow(dccc))
    simback2 <- rep(NA, nrow(dccc))
    simback3 <- rep(NA, nrow(dccc))
    simback4 <- rep(NA, nrow(dccc))
    simback5 <- rep(NA, nrow(dccc))
    
    for(i in 2:nrow(dccc)){
        if (dccc$entry[i] %in% rownames(ancos) && dccc$entry[i-1] %in% rownames(ancos)){
            if (dccc$sid[i] == dccc$sid[i-1]){
                simback1[i] <- ancos[toString(dccc$entry[i]), toString(dccc$entry[i-1])]
            }
        }
    }
    
    for(i in 3:nrow(dccc)){
        if (dccc$entry[i] %in% rownames(ancos) && dccc$entry[i-2] %in% rownames(ancos)){
            if (dccc$sid[i] == dccc$sid[i-2]){
                simback2[i] <- ancos[toString(dccc$entry[i]), toString(dccc$entry[i-2])]
            }
        }
    }
    
    for(i in 4:nrow(dccc)){
        if (dccc$entry[i] %in% rownames(ancos) && dccc$entry[i-3] %in% rownames(ancos)){
            if (dccc$sid[i] == dccc$sid[i-3]){
                simback3[i] <- ancos[toString(dccc$entry[i]), toString(dccc$entry[i-3])]
            }
        }
    }
    
    for(i in 5:nrow(dccc)){
        if (dccc$entry[i] %in% rownames(ancos) && dccc$entry[i-4] %in% rownames(ancos)){
            if (dccc$sid[i] == dccc$sid[i-4]){
                simback4[i] <- ancos[toString(dccc$entry[i]), toString(dccc$entry[i-4])]
            }
        }
    }
    
    for(i in 6:nrow(dccc)){
        if (dccc$entry[i] %in% rownames(ancos) && dccc$entry[i-5] %in% rownames(ancos)){
            if (dccc$sid[i] == dccc$sid[i-5]){
                simback5[i] <- ancos[toString(dccc$entry[i]), toString(dccc$entry[i-5])]
            }
        }
    }
    
    return(list(simback1,simback2,simback3,simback4,simback5))
    
}