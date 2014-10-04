# Ignore header lines. Do names by hand
pm0 <- read.table("RD_501_88101_1999-0.txt", comment.char="#", header=FALSE, sep="|", na.strings="")
cnames<-readLines("RD_501_88101_1999-0.txt", 1)
# Got a single string. Split gives real result in element [[1]]
cnames<-strsplit(cnames,"|", fixed=TRUE)
# Use make.names to get rid of spaces etc
names(pm0) <- make.names(cnames[[1]])
names(pm0)
x0 <- pm0$Sample.Value
# Look at y-values and proportion of missing data.
summary(x0)
mean(is.na(x0))
# Much more data for 2012, and less missing data
pm1 <- read.table("RD_501_88101_2012-0.txt", comment.char="#", header=FALSE, sep="|", na.strings="")
dim(pm1)
names(pm1) <-names(pm0)
x1 <- pm1$Sample.Value
summary(x1)
mean(is.na(x1))
boxplot(log10(x0),log10(x1))
# Note warnings for neg values. How many?
negative <- x1 < 0 # logicals
mean(negative, na.rm=TRUE) # small %age
dates <- pm1$Date # Just integers!
dates <- as.Date(as.character(dates), "%Y%m%d")
hist(dates, "month")
hist(dates[negative], "month") # Note use of logical vector
# Neg values happening in winter when values low - meas error
# Look at one monitor, in both years. Use paste to combine county and
# site in one key. Look in NY state.
intersect(site0, site1)
site0 <- unique(subset(pm0, State.Code == 36, c(County.Code, Site.ID)))
site0 <- paste(site0[,1], site0[,2], sep=".")
site1 <- unique(subset(pm1, State.Code == 36, c(County.Code, Site.ID)))
site1 <- paste(site1[,1], site1[,2], sep=".")
intersect(site0, site1)
both <- intersect(site0, site1)
# Get subsets of pm0 and pm1 in both. Add "key" column to do that.
pm0$county.site <- with(pm0, paste(County.Code, Site.ID, sep="."))
cnt0 <- subset(pm0, State.Code == 36 & county.site %in% both)
pm1$county.site <- with(pm1, paste(County.Code, Site.ID, sep="."))
cnt1 <- subset(pm1, State.Code == 36 & county.site %in% both)
# Find a countysite with a decent number of rows.
sapply(split(cnt0, cnt0$county.site), nrow)
sapply(split(cnt1, cnt1$county.site), nrow)
# Pick 63.2008
pm0sub <- subset(pm0, State.Code == 36 & County.Code == 63 & Site.ID == 2008)
pm1sub <- subset(pm1, State.Code == 36 & County.Code == 63 & Site.ID == 2008)
dates1 <- as.Date(as.character(pm1sub$Date), "%Y%m%d")
x1sub <- pm1sub$Sample.Value
plot(dates1,x1sub)
dates0 <- as.Date(as.character(pm0sub$Date), "%Y%m%d")
plot(dates0,x0sub)
# Only half of year collected for 1999
# Try to compare. Note axes not scaled together - diff ranges
par(mfrow=c(1,2), mar = c(4,4,2,1))
plot(dates0,x0sub)
abline(h = median(x0sub, na.rm=T))
plot(dates1,x1sub)
abline(h = median(x1sub, na.rm=T))
# Try compare with common range on y-axis
rng <- range(x0sub, x1sub, na.rm=T) # Do both at once
par(mfrow=c(1,2), mar = c(4,4,2,1))
plot(dates0, x0sub, pch = 20, ylim = rng)
abline(h = median(x0sub, na.rm=T))
plot(dates1, x1sub, pch = 20, ylim = rng)
abline(h = median(x1sub, na.rm=T))
# Now try states. Which better, which worse? Use tapply for split/reduce.
mn0 <- with(pm0, tapply(Sample.Value, State.Code, mean, na.rm=T))
mn1 <- with(pm1, tapply(Sample.Value, State.Code, mean, na.rm=T))
summary(mn0)
summary(mn1)
d0 <- data.frame(state=names(mn0), mean=mn0)
d1 <- data.frame(state=names(mn1), mean=mn1)
mrg <- merge(d0, d1, by = "state")
# He has 52 obs, I have 53 ??? Note use of rep to put points on right year
# Using 1998 and 2013 is just for tidiness
par(mfrow=c(1,1))
with(mrg, plot(rep(1999,53), mrg[,2], xlim = c(1998,2013)))
points(rep(2012,53), mrg[,3])
# Segments puts in lines to show movement.
# Should have controlled y limits on plot based on range(mrg[,2], mrg[,3]) see above.
segments(rep(1999,53), mrg[,2], rep(2012,53), mrg[,3])
