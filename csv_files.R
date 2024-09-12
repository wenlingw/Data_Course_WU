#assignment 2 video

getwd()
list.files()[1:10]
list.files(pattern = "x")
list.files(path = "C:/Users/maggi/Desktop/BIOL3100/Data_Course_WU", 
           recursive = TRUE, 
           pattern = ".csv")
mypath <- "C:/Users/maggi/Desktop/BIOL3100/Data_Course_WU/Data/"
mypath
list.dirs(path = mypath, recursive = FALSE)
data_directories <- list.dirs(path = mypath, recursive = FALSE)
data_directories[3]
list.files(path = 'Data', full.names = TRUE)
dir.exists(":C/Users/maggi/Desktop/BIOL3100/Data_course_WU/Data/flights")
list.files(path = data_directories[3],full.names = TRUE)
file.create(file.path(data_directories[3],"testfile"))
list.files(path = data_directories[3],full.names = TRUE)
file.remove("C:/Users/maggi/Desktop/BIOL3100/Data_Course_WU/Data/flights/testfile")

getwd()
list.files()[1:10]
list.files(path = "..", full.names = TRUE)
list.files(path = "../Data_Course_WU/Assignments")
mypath <- "C:/Users/maggi/Desktop/BIOL3100/Data_Course_WU/Data/"
fastas <- list.files(mypath,recursive = TRUE,pattern = "*5.8S*.fasta$",full.names = TRUE)
fastas
list.dirs(path = mypath, recursive = FALSE)

#assignment2
x = list.files(path = mypath,
           recursive = TRUE,
           pattern = ".csv")
length(x)
df <- read.csv("Data/wingspan_vs_mass.csv")
?head
head(df, n=5)

b <- list.files(path = mypath,
           recursive = TRUE,
           pattern = "^b")
head(b, n=1, recursive = TRUE)
for (i in b) {
  print(readLines('Data/data-shell/creatures/basilisk.dat', n = 1))
  print(readLines('Data/data-shell/data/pdb/benzaldehyde.pdb', n = 1))
  print(readLines('Data/Messy_Take2/b_df.csv', n = 1))
}

readLines('Data/data-shell/creatures/basilisk.dat', n = 1)
readLines('Data/data-shell/data/pdb/benzaldehyde.pdb', n = 1)
readLines('Data/Messy_Take2/b_df.csv', n = 1)
