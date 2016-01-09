plot3fun <- function(){
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
	png("plot3.png")
	#ensures 1 graph per page
	par(mfrow = c(1,1))
	#creates a line graph forcing sub_metering_1 from factor class to character and then numeric,
	#respectively. Also creates a label for the Y axis.
	plot(dat2$DTcomb, as.numeric(as.character(dat2$Sub_metering_1)), type = "l", 
		ylab = "Energy sub metering", xlab ="")
	#adds another line on the graph already created using data from Sub_metering_2 and coloring that line red.
	points(dat2$DTcomb, as.numeric(as.character(dat2$Sub_metering_2)), type = "l", col = "red")
	#adds another line on the graph already created using data from Sub_metering_3 and coloring that line blue.
	points(dat2$DTcomb, as.numeric(as.character(dat2$Sub_metering_3)), type = "l", col = "blue")
	#creates a legend for the graph, putting it in the top right hand corner displaying the labels of each
	#line and the color that each line represents. 
	legend("topright", c("Sub_metering_1", "Sub_metering_2", "Sub_metering_3"), 
		lty =c(1,1,1), col = c("black", "red", "blue"))

	#closes the graphics device opened to write to .png and returns to default graphics device.
	dev.off()

}