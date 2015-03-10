time_plot <- function(df, country="USA", location_var="iso3", main="", data=F){
    needed_vars <- c("year", "age", "y", "yhat", location_var)
    if(any(!(needed_vars %in% names(df)))){
        stop(paste0("The following variables are missing from the data frame: ",
                    paste(needed_vars[!(needed_vars %in% names(df))],
                          collapse=", ")))
    }
    xlab <- "Forecats"
    if(data){
        df[,"yhat"] <- df[,"y"]
        xlab <- "Observed"
    }
    sub_df <- df[df[,location_var] == country,]
    sub_df <- sub_df[order(sub_df$age, sub_df$year),]
    plot.time <- ggplot(sub_df, aes(x=age, y=yhat, color=year, group=year)) +
        geom_line() + theme_bw() + scale_x_continuous("Age") + 
        scale_y_continuous(xlab) +  ggtitle(paste0(main, ", ", country))
    plot.time <- plot.time + scale_color_gradientn("Time", colours=rainbow(7)) + 
        theme(legend.justification=c(0,1), legend.position=c(0.05,1),
              legend.direction="horizontal", legend.text=element_text(angle=45),
              legend.title.align=1,
              legend.background = element_rect(fill="transparent"))
    plot.time
}