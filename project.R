project <- function(null) {
	# Read in all the tables
	features<-read.table("features.txt")
	activity_labels<-read.table("activity_labels.txt")

	train.subject_train<-read.table("train/subject_train.txt")
	train.X_train<-read.table("train/X_train.txt")
	train.y_train<-read.table("train/y_train.txt")

	test.subject_test<-read.table("test/subject_test.txt")
	test.X_test<-read.table("test/X_test.txt")
	test.y_test<-read.table("test/y_test.txt")

	# Relabel the columns in the data sets
	colnames(test.X_test)<-features[,2]
	colnames(train.X_train)<-features[,2]

	# Add indexes
	train.subject_train<-index(train.subject_train)
	train.X_train<-index(train.X_train)
	train.y_train<-index(train.y_train)
	test.subject_test<-index(test.subject_test)
	test.X_test<-index(test.X_test)
	test.y_test<-index(test.y_test)

	# Merge the activities and the descriptions
	mtest <- merge(test.y_test,activity_labels,by.x="V1",by.y="V1",all=TRUE)
	mtest <- mtest[order(mtest$id),]
	mtrain <- merge(train.y_train,activity_labels,by.x="V1",by.y="V1",all=TRUE)
	mtrain <- mtrain[order(mtrain$id),]

	# Add in activities and test subjects

	testdata <- cbind(test.X_test,mtest,test.subject_test)
	traindata <- cbind(train.X_train,mtrain,train.subject_train)

	# Pull out only the means and standard deviations
	fintest<-testdata[c(1:6,41:46,81:86,121:126,161:166,201,202,214,215,227,228,240,241,253,254,266:271,345:350,424:429,503,504,516,517,529,530,542,543,565,566)]
	fintrain<-traindata[c(1:6,41:46,81:86,121:126,161:166,201,202,214,215,227,228,240,241,253,254,266:271,345:350,424:429,503,504,516,517,529,530,542,543,565,566)]

	# Relabel the Activity and Subject columns in the combined table
	colnames(fintest)[67:68] <- c("Activity","Subject")
	colnames(fintrain)[67:68] <- c("Activity","Subject")

	# Combine tables. This is the 
	final <- rbind(fintest,fintrain)
	finalsort <- final[order(final$Subject,final$Activity),]

	# Split it by Subject and then by Activity
	sfinalsort <- split(finalsort,list(finalsort$Subject,finalsort$Activity))

	# Caching the names of the individual lists, as the upcoming lapply will wipe them out.
	listnames <- names(sfinalsort)

	# This is the money function. Column means across all 180 Activity/Subject combos. Booya baby.
	booya <- lapply(1:180, function(x) colMeans(sfinalsort[[x]][,1:66]))

	# Reapplying the names from the previous split, as the lapply wiped them out. 
	names(booya) <- listnames

	# Final writing out of the table. 
	write.table(booya,file="booya.txt", row.name=FALSE)
}

# Indexing Function 
index<-function(x){
	q <- nrow(x)
	y <- cbind(x,1:q)
	colnames(y)[ncol(y)]<-"id"
	x<-y
}
