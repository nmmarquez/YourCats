age_plot <- function(df, country="USA", location_var="iso3", main=""){
    needed_vars <- c("year", "age", "y", "yhat", location_var)
    if(any(!(needed_vars %in% names(df)))){
        stop(paste0("The following variables are missing from the data frame: ",
                    paste(needed_vars[!(needed_vars %in% names(df))],
                          collapse=", ")))
    }
    sub_df <- df[df[,location_var] == country,]
    sub_df <- sub_df[order(sub_df$age, sub_df$year),]
    plot.age <- ggplot(sub_df, aes(x=year, y=yhat, color=age, group=age)) + 
        geom_line() + theme_bw()  + scale_x_continuous("Time") + 
        scale_y_continuous("Data and Forecats") + 
        ggtitle(paste0(main, ", ", country))
    plot.age <- plot.age + scale_color_gradientn("Age",colours=rainbow(7)) + 
        theme(legend.margin=unit(-0.02,"npc"),legend.text=element_text(size=8))
    plot.age <- plot.age + 
        geom_path(data=sub_df,aes(x=year,y=y, color=age, group=age),
                  linetype="dashed",na.rm=TRUE)
    plot.age
}