extract_dims<- function(data.set){
        ## From a data set of a chart data, extract dimensions from radiology reports
        library(dplyr)
        # Create regex pattern that identifies #.# x #.# x #.#
        regex.pattern<- "(\\d+\\.\\d+\\s?)x(\\s?\\d+\\.\\d+\\s?)x(\\s?\\d+\\.\\d+)"
        #Subset only the rows containing data with this format
        rad.subset<- data.set[grep(pattern = regex.pattern, x = data.set$Contents),]
        #Subset data from MRIs only
        rad.subset.mri<- rad.subset[grep('MR', rad.subset$Contents),]
        #Add a new column with just the dims and select the relevant columns
        extracted.dims<- unlist(str_extract(string = rad.subset.mri$Contents, pattern = regex.pattern))
        rad.subset.dims<- mutate(rad.subset.mri, dimensions = extracted.dims)
        #OLD CODE: rad.subset.dims<-cbind.data.frame(rad.subset.mri, extracted.dims, stringsAsFactors=FALSE)
        rad.subset.dims<- select(rad.subset.dims, original.order, MRN, Event.Date, dimensions)
        #Clean dates
        ##Define pattern
        regex.pattern.date<-"(\\d{4}-\\d{2}-\\d{2})"
        ##create clean vector
        clean.dates<- str_extract(string = rad.subset.dims$Event.Date, regex.pattern.date)
        ##mutate the original df
        rad.subset.dims<-mutate(rad.subset.dims, Event.Date = clean.dates)
        
        #Seperate the dimensions into a new dataframe
        ##First a regex patterns that looks for the x (plus or minus a space)
        regex.pattern.x<- "\\s?x\\s?"
        ##Create a list containing all the dimensions
        list.dims<-str_split(rad.subset.dims$dimensions, regex.pattern.x)
        ##Create a new dataframe using the list items
        ##using the list's length to define the number of rows
        df<- data.frame(matrix(unlist(list.dims), nrow = length(list.dims), byrow = T))
        ##Combine back into the rad.subset.dims df, after renaming cols and convert to numeric
        names(df)<-c("x","y","z")
        df$x<-as.numeric(levels(df$x))[df$x]
        df$y<-as.numeric(levels(df$y))[df$y]
        df$z<-as.numeric(levels(df$z))[df$z]
        rad.subset.dims<-cbind(rad.subset.dims, df)
        
        #Calculate the volume into a new col
        rad.subset.dims<- mutate(rad.subset.dims, vol = 4/3*rad.subset.dims$x*rad.subset.dims$y*rad.subset.dims$z*pi)
        
        return(rad.subset.dims)
}

