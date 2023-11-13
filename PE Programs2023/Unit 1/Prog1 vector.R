subject_name<- c("John Doe", "Jane Doe", "Steve Graves") #vector to store patient name
temperature<- c(98.1, 98.6, 101.4) #vector to store temperature of patient
flu_status <- c(FALSE, FALSE, TRUE) #vector to store status of flu
temperature[2] #print temp of Jane Doe
temperature[2:3] #print temperature with in certain range
temperature[-2] #exclude the temperature of the second patient
temperature[c(TRUE, TRUE, FALSE)] #print the temperature of those pateints where value is TRUE
