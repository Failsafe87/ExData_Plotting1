plot1fun <- function(){
	#reads in the table in to a data.frame
	dat<-read.table("household_power_consumption.txt",header = TRUE, sep= ";")
	#puts the columns Date and Time together into one seperate column called DTcomb 
	dat$DTcomb <- paste(dat$Date, dat$Time)
	#After the date and time have been combined, they turn in to a character class,
	#this converts the character into date and time class.
	dat$DTcomb <- strptime(dat$DTcomb, "%d/%m/%Y %H:%M:%S")
	#pulls out all values of the DTcomb column which satisfy the conditions
	beg <- which(dat$DTcomb == strptime("2007-02-01", "%Y-%m-%d"))
	end <- which(dat$DTcomb == strptime("2007-02-02 23:59:00", "%Y-%m-%d %H:%M:%S"))
	#creates a new data.fame that stores all the values of dat during the specific time frame
	#we want to extrapolate our data from.
	dat2 <- dat[beg:end,]
	#opens up the .png graphics device to write the plot onto
	png("plot1.png")
	#ensures 1 graph per page
	par(mfrow = c(1,1))
	#creates a histogram and forces Global_active_power from factor class to character and then numeric class
	#respectively. Gives the graph a title and lables the X-axis. ALso adds the Color red to the hist.
	hist(as.numeric(as.character(dat2$Global_active_power)), main= "Global Active Power", 
		xlab = "Global Active Power (kilowatts)", col = "red")
	#closes the graphics device opened to write to .png and returns to default graphics device.
	dev.off()
}