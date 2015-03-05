write_cats <- function(df, age_var="age", loc_var="location_id", time="year",
                       stub="mort", sex_id=1, base=paste0(getwd(), "/"), 
                       no_vars=c("location_id", "iso3", "age_group_id", 
                                 "year", "sex")){
    df <- subset(df, sex == sex_id)
    #df[,"location_id"] <- df[,"location_id"] + 1000
    df[,loc_var] <- sprintf("%04d", df[,loc_var])
    #df[,"age_group_id"] <- df[,"age_group_id"] + 10
    df[,age_var] <- sprintf("%04d", df[,age_var]) 
    df[, "time"] <- df[,time] - min(df[,time]) + 1
    df <- df[order(df[,loc_var], df[,age_var], df[,time]),]
    vars <- names(df)[-which(names(df) %in% no_vars)]
    
    for(loc in unique(df[,loc_var])){
        for(age in unique(df[,age_var])){
            sub_df <- df[df[,age_var] == age & df[,loc_var] == loc,]
            rownames(sub_df) <- sub_df[,time]
            sub_df[,time] <- NULL
            if(nrow(sub_df) != 34){print(paste(loc, age, sep=" "))}
            if (nrow(sub_df) > 0){
                write.table(sub_df[,vars], 
                            file=paste0(base, stub, loc, age, ".txt"))
            }
        }
    }
}
