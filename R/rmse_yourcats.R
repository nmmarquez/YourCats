rmse_yourcats <- function(yc){
    n <- length(array.yourcast(yc))
    groupN <- ncol(array.yourcast(yc)[[1]][,,1])
    in_years <- as.character(yc$aux$sample.frame[1]:yc$aux$sample.frame[2])
    out_years <- as.character(yc$aux$sample.frame[3]:yc$aux$sample.frame[4])
    n_in <- n * groupN * length(in_years)
    n_out <- n * groupN * length(out_years)
    array_yc <- array.yourcast(yc)
    rmse <- rowSums(sapply(1:n, function(i) 
        rowSums((array_yc[[i]][,,"yhat"] - array_yc[[i]][,,"y"])**2)))
    rmse <- c(sum(rmse[in_years]/n_in), sum(rmse[out_years]/n_out))
    names(rmse) <- c("in", "out")
    rmse
}