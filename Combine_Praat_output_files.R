setwd("C:/Users/jerry/Documents/Cathryn_Projects/Praat/Speaker_Files")

library("tidyverse")

# Number of files

numfiles <- 5

# read in the files

for (i in 1:numfiles) {
  
  file_name <- paste("Speaker", as.character(i),".txt",sep = "")
  
  if(i==1){
    my_data <- read_tsv(file_name)
  } else {
    temp <- read_tsv(file_name)
    my_data <- bind_rows(my_data,temp) }
}

#  Write out the collected praat results

write_excel_csv(my_data,path = 
                  "C:/Users/cathr/OneDrive/Documents/ICPhS_Mandarin/praat_data.csv",
                col_names = TRUE  )
  


