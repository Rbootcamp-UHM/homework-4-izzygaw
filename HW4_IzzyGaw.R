### Script to analyze spectrophotometer files
### Written by Izzy Gaw

setwd("C:/Users/izzyg/Documents/rbootclass/homework-4-izzygaw")
#require(rmarkdown)
#Sys.setenv(RSTUDIO_PANDOC="C:/Program Files/RStudio/bin/pandoc")
#render("Homework4_IzzyGaw.Rmd", output_format="html_document") # to html

read.spec <- function ( myfile )  {  
	dat <- read.table(file=myfile, skip=17, comment.char=">")
	names(dat) <- c("lambda", "intensity")
	
	dat <- dat[ dat$lambda >= 300 & dat$lambda <= 700,   ]
	return (dat)
}


dat <- read.spec("Data/20070725_01forirr.txt")

plot.spec <- function( X ) {

    plot(X, type="l")
	minten<- max(X$intensity, na.rm=TRUE)
	lam <- X[X$intensity==minten, "lambda"]
	points(x=lam, y=minten,cex=3,pch=19,col="red")
		multi_return <- function (Y) {
		my_list <- list (lam, minten)
		return (my_list) 
		}
		multi_return()
} 
plot.spec(dat)

setwd("C:/Users/izzyg/Documents/rbootclass/homework-4-izzygaw/Data")

myfiles <- list.files()
myfiles

class(myfiles)

txt <- c(myfiles)
if(length(i <- grep(".txt", txt)))
   cat( txt, "C:/Users/izzyg/Documents/rbootclass/homework-4-izzygaw/Data")
i 
txt[i] #there are 38 .txt files

myfiles <- c(txt[i])
myfiles

####4 and 5
#myfiles <-c(paste0("Data/", myfiles)) #doesn't work with this line :(
#myfiles

class(myfiles)
   
datalist = lapply(myfiles, function(x)read.delim(x, header=T)) 
datalist

bloop<- data.frame(datalist)
bloop #wont do all the rows?

length(datalist)
list

#ideas for 6..

####6####
setwd("C:/Users/izzyg/Documents/rbootclass/homework-4-izzygaw")

myfiles

YUP <- function() {

	data <- read.spec("Data/"))
	plot.spec(data)
}

YUP(data)

#ideas for 6..
mylength <- length(myfiles)
mylength

dataf <- data.frame(matrix(0, nrow=mylength, ncol=3))
dataf

columnheadings <- c("File", "Lambda", "Maximum Intensity")
names(dataf) <- columnheadings
dataf


plot(dat, type="l")

## What should we return for our final dataframe of max intensity, lambda?

plot(dat, type="l")   ## ... 