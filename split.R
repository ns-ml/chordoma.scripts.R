split_patients<- function(df){
        number.of.patients<-length(unique(df$MRN))
        X <- split(df, df$MRN)
        Y <- lapply(seq_along(X), function(x) as.data.frame(X[[x]]))
        patients<- paste("patient", seq(1:number.of.patients), sep="")
        names(Y) <- patients
        list2env(Y, envir = .GlobalEnv)
}