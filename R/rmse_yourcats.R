rmse_yourcats <- function(yc){
    n <- length(array.yourcast(yc))
    groupN <- ncol(array.yourcast(yc)[[1]][,,1])
    in_years <- as.character(yc$aux$sample.frame[1]:yc$aux$sample.frame[2])
    out_years <- as.character(yc$aux$sample.frame[3]:yc$aux$sample.frame[4])
    array_yc <- array.yourcast(yc)
    y_in <- c(sapply(array_yc, function(x)c(x[in_years,,1])))
    y_out <- c(sapply(array_yc, function(x)c(x[out_years,,1])))
    y_hat_in <- c(sapply(array_yc, function(x)c(x[in_years,,2])))
    y_hat_out <- c(sapply(array_yc, function(x)c(x[out_years,,2])))
    rmse_func <- function(y, yhat){
        (sum((yhat - y)**2)/ length(y))**.5
    }
    rmse <- c(rmse_func(y_in, y_hat_in), rmse_func(y_out, y_hat_out))
    names(rmse) <- c("in", "out")
    rmse
}