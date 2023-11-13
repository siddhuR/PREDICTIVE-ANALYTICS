dataframe <- data.frame(name <- c("John", "Mary", "Hyka"), 
                        age <- c(20, 21, 22),
                        course <- c("cse", "ece","eee"),
                        stringsAsFactors = "FALSE")
dataframe
dataframe[1] #first row
dataframe[1,2] #first row second column
dataframe[c(1,2), c(2,3)] #first row second row with second column and third column
