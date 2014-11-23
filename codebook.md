require(data.table)
require(reshape2)
require(knitr)

#Download and unzip file

url <- "https://d396qusza40orc.cloudfront.net/getdata%2Fprojectfiles%2FUCI%20HAR%20Dataset.zip"
f <- "Dataset.zip"
path <- getwd()
download.file(url, file.path(path, f))
unzip(f)
pathIn <- file.path(path, "UCI HAR Dataset")

#Read documents

strain <- fread(file.path(pathIn, "train", "subject_train.txt"))
stest  <- fread(file.path(pathIn, "test" , "subject_test.txt" ))

atrain <- fread(file.path(pathIn, "train", "Y_train.txt"))
atest  <- fread(file.path(pathIn, "test" , "Y_test.txt" ))

train <- (data.table(read.table(file.path(pathIn, "train", "X_train.txt"))))
test  <- (data.table(read.table(file.path(pathIn, "test" , "X_test.txt" ))))

#1 Crate 1 set

#Merge rows 

subject <- rbind(strain, stest)
setnames(subject, "V1", "subject")

activity <- rbind(atrain, atest)
setnames(activity, "V1", "aNum")

dt <- rbind(train, test)

#Merge Columns
subject <- cbind(subject, activity)
dt <- cbind(subject, dt)

setkey(dt, subject, aNum)

#2 Extract means and sd

features <- fread(file.path(pathIn, "features.txt"))
setnames(features, names(features), c("fNum", "Name"))

features <- features[grepl("mean\\(\\)|std", Name)]

features$fCode <- features[, paste0("V", fNum)]
select <- c(key(dt), features$fCode)
dt <- dt[, select, with=FALSE]

#3 Activity Names

anames <- fread(file.path(pathIn, "activity_labels.txt"))
setnames(anames, names(anames), c("aNum", "aName"))

dt <- merge(dt, anames, by="aNum", all.x=TRUE)

setkey(dt, subject, aNum, aName)

#4 Melt and Merge

dt <- data.table(melt(dt, key(dt), variable.name="fCode"))
dt <- merge(dt, features[, list(fNum, fCode, Name)], by="fCode", all.x=TRUE)

#5 Create Tidy

dt$Name <- as.factor(dt$Name)

a <- grepl("mean()", dt$Name)
b <- grepl("SD()", dt$Name)
x <- cbind(a,b)
y <- matrix(seq(1, 2), nrow=2)
dt$fVariable <- factor(x %*% y, labels=c("Mean", "SD"))

setkey(dt, subject, aName, fVariable)
dtTidy <- dt[, list(count = .N, average = mean(value)), by=key(dt)]
setnames(dtTidy, c("Subject", "Activity","Variable","Count","Average"))


#Write results in table

write.table(dtTidy, file = "tidy.txt", sep = "\t\t", row.name=FALSE)

#Make Codebook

knit("run_analysis.R", output="codebook.md", encoding="ISO8859-1", quiet=TRUE)
