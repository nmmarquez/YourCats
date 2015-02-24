ihme_region_plot <- function(yc_object, region, age, use_super_region=F, ...){
    age <- as.character(age + 10)
    possible <- paste0(.libPaths(), "/YourCats")
    path <- possible[which(file.exists(possible))[1]]
    path <- paste0(path, "/data/location_hierarchy.csv")
    loc_df <- read.csv(path)
    loc <- c("region", "super_region")[use_super_region + 1]
    locations <- loc_df[loc_df[,loc] == region, "location_id"] + 1000
    preds <- lapply(as.character(locations), 
                    function(x) array.yourcast(yc_object)[[x]][,,2])
    data <- lapply(as.character(locations), 
                   function(x) array.yourcast(yc_object)[[x]][,,1])
    start <- min(as.numeric(yc_object$call$sample.frame))
    end <- max(as.numeric(yc_object$call$sample.frame))
    forcats <- as.numeric(yc_object$call$sample.frame)[2]
    ymin <- min(sapply(c(preds, data), function(x) min(x[,age])))
    ymax <- max(sapply(c(preds, data), function(x) max(x[,age])))
    plot(c(), c(), ylim=c(ymin, ymax), xlim=c(start, end), xlab="Years", 
         ylab="forecats", ...)
    cl <- rainbow(length(preds))
    for(i in 1:length(preds)){
        lines(start:end, preds[[i]][,age], col=cl[i])
        lines(start:end, data[[i]][,age], col=cl[i], type="b", pch=19, cex=.5)
    }
    legend("bottomleft", legend=c("predicted", "actual"), 
           col=1, lty=c(1,2), pch=c(NA, 19), cex=.5)
}