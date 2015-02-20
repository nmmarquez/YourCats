write_cats <- function(df, stub="mort", sex_id=1, base=paste0(getwd(), "/"), 
                       no_vars=c("location_id", "iso3", "age_group_id", 
                                 "year", "sex")){
    df <- subset(df, sex == sex_id)
    df[,"location_id"] <- df[,"location_id"] + 1000
    df[,"location_id"] <- sprintf("%04d", df$location_id)
    df[,"age_group_id"] <- df[,"age_group_id"] + 10
    df[,"age_group_id"] <- sprintf("%02d", df$age_group_id) 
    df[, "time"] <- df$year - 15
    df <- df[with(df, order(location_id, age_group_id, year)),]
    vars <- names(df)[-which(names(df) %in% no_vars)]
    
    for(loc in unique(df$location_id)){
        for(age in unique(df$age_group_id)){
            sub_df <- subset(df, age_group_id == age & location_id == loc)
            rownames(sub_df) <- sub_df$year
            sub_df[,"year"] <- NULL
            
            if (nrow(sub_df) > 0){
                write.table(sub_df[,vars], 
                            file=paste0(base, stub, loc, age, ".txt"))
            }
        }
    }
}