### Script to Analyze Spectrophotometer Files
### Written by Izzy Gaw

#Please change your working directory where your data files are stored below and uncomment when complete...

#setwd("C:/Users/izzyg/Documents/rbootclass/homework-4-izzygaw") 

########################
##Function Definitions##
########################

#creates a path to your "Data" folder and selects all files that have .txt
myfiles <- list.files(path = "Data",pattern="*.txt") 

#this function reads files and returns the data from those files
read.spec <- function (myfiles){
	dat <- read.table(file=myfiles, skip=17, comment.char=">")
	names(dat) <- c("lambda", "intensity")
	dat <- dat[ dat$lambda >= 300 & dat$lambda <=700,  ] 
	return(dat)
}

#this function plots data you want to input
plot.spec <- function (X) {
	plot(X, type="l")
	max.int <- max(X$intensity, na.rm=TRUE) #finds maximum intensity
	l <- X[X$intensity == max.int, "lambda"] #finds lambda at maximum intensity
	points(x=l,y=max.int,col="purple",cex=3,pch=19) #creates a purple dot at max.int and l
}

######################
#####Working Code#####
######################

#finds amount of files in your data folder
filelength <- length(myfiles) #38 files total

#created an empty data frame to store data in later
dataf <- data.frame(matrix(data=NA, nrow = filelength, ncol = 4, dimnames = list(c(1:38))))

#create column labels
columnnheadings <- c("","File","Lambda","Maximum Intensity") 

#adding file numbers
fileN <- cbind(data.frame = c(1:38))

#add each label to our empty data frame
names(dataf) <- columnnheadings

#this function will start creating a PDF, choose TRUE/FALSE for onefile depending on if you want a single or multiple files
pdf(file=paste("Homework4_SpecGraphs_IzzyGaw", ".pdf", sep=""), onefile=TRUE) 

for (i in 1:filelength) { #for loop to read in a folder with data and create a csv and a pdf
	dat<- read.spec(paste0("Data/", myfiles[i])) #adds "Data/" to all my files so they are more easily read
	plot.spec(dat)
	max.int <- max(dat$intensity, na.rm=TRUE) 
	l <- dat[dat$intensity == max.int, "lambda"]
		legend("topleft", legend = max.int, col = "blue",  #creates a legend for maximum intensity
          ncol = 2, cex = 1, text.col = "blue", title="Maximum Intensity")
		legend("bottomright", legend = l, col = "red",
          ncol = 2, cex = 1, text.col = "red", title="Lambda at Maximum Intensity")  #creates a legend for lambda at maximum intensity
	dataf[i,] <- c(fileN[i], myfiles[i], l, max.int)	#adds all the data to the data frame
	
}
	dev.off() #turns off pdf creator
	
#the below line will create excel file of all of your data from the dataframe you created above, follow commented line below for further instructions...
#if you want to change the name of you new excel file: write.csv(dataf, "C:/...\\file-name-goes-here.csv", row.names=FALSE), then uncomment the below line

#write.csv(dataf,"C:/Users/izzyg/Documents/rbootclass/homework-4-izzygaw\\HW4IzzyGaw.csv", row.names=FALSE) 

#Run the whole code at once for an excellent coding experience!
