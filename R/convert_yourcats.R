convert_yourcats <- function(yc){
    loc_df <- yc$aux$G.names
    age_df <- yc$aux$A.names
    age_df[,2] <- as.numeric(age_df[,2]) 
    locations <- sapply(strsplit(names(yc$yhat), split=""), function(x)
        paste(x[1:4], collapse = ""))
    ages <- sapply(strsplit(names(yc$yhat), split=""), function(x)
        paste(x[5:8], collapse = ""))
    for(i in 1:length(yc$yhat)){
        yc$yhat[[i]] <- as.data.frame(yc$yhat[[i]])
        yc$yhat[[i]]$year <- row.names(yc$yhat[[i]])
        yc$yhat[[i]][,names(age_df)[1]] <- ages[i]
        yc$yhat[[i]][,names(loc_df)[1]] <- locations[i]
    }
    df <- do.call("rbind", yc$yhat)
    row.names(df) <- NULL
    df <- merge(df, loc_df); df <- merge(df, age_df)
    df$year <- as.numeric(df$year)
    df
}