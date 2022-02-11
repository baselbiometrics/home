path <- getwd()

# list all talks in a directory
tab <- data.frame(list.files(paste(path, "/talks/20141002", sep = "")))
tab <- tab[tab[, 1] != "agenda.pdf", ]
dimnames(tab) <- NULL
write.table(tab, file = "C:/Users/rufiback/Desktop/xxx.csv", row.names = FALSE, col.names = FALSE)

library(PBSmodelling)
openFile(fname = "C:/Users/rufiback/Desktop/xxx.csv")




# list all talks in a directory
tab <- data.frame(list.files(paste(path, "/trainings/20210202", sep = "")))
tab <- tab[tab[, 1] != "agenda.pdf", ]
dimnames(tab) <- NULL
write.table(tab, file = "C:/Users/rufiback/Desktop/xxx.csv", row.names = FALSE, col.names = FALSE)

library(PBSmodelling)
openFile(fname = "C:/Users/rufiback/Desktop/xxx.csv")

