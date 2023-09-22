

DayNight <- function (indates, lon, lat )
	{
	stopifnot ( 'POSIXct' %in% class(indates))
	
	x2 <- Sunrise (indates, lon, lat, uniq = FALSE)
	x3 <- Sunset (indates, lon, lat, uniq = FALSE)
	
	time_after_rise <- indates - x2
	time_after_set <- indates - x3

	daynight <- time_after_rise
	daynight[] <- 'day'

	daynight[time_after_rise < 0] <- 'night'
	daynight[time_after_set > 0] <- 'night'

	#to override difftime
	daynight <- as.character (daynight)

	return (daynight)
	}

#TimeAfterSunrise (x1a, 1,54)








































Sunrise <- function ( indates, lon, lat, uniq = TRUE )
	{
	#timezone = timezone
	stopifnot ( class (indates) == 'Date' | 'POSIXct' %in% class (indates))

	#force indates to Date formate
	indates = as.Date( indates )
	dates <- as.POSIXct ( indates )
		
	x1 = yday(indates)

	#in hours
	x2 <- suncalc ( x1, lat, lon )
		
	
	sunrise_seconds <- round (x2$sunrise * 60 * 60, 0)
	sunrise <- dates + sunrise_seconds
	
	if (uniq) {sunrise <- unique (sunrise)}
	return (sunrise)
	}

















































Sunset <- function ( indates, lon, lat, uniq = TRUE )
	{
	#timezone = timezone
	stopifnot ( class (indates) == 'Date' | 'POSIXct' %in% class (indates))
	
	#force indates to Date formate
	indates = as.Date( indates )
	dates <- as.POSIXct ( indates )
	
	x1 = yday(indates)

	#in hours
	x2 <- suncalc ( x1, lat, lon )
		
	
	sunset_seconds <- round (x2$sunset * 60 * 60, 0)
	sunset <- dates + sunset_seconds
	
	if (uniq) {sunset <- unique (sunset)}
	return (sunset)
	}






























































SolarNoon <- function ( indates, lon, lat, uniq = TRUE )
	{
	#timezone = timezone
	stopifnot ( class (indates) == 'Date' | 'POSIXct' %in% class (indates))
	
	
	#force indates to Date formate
	indates = as.Date( indates )
	#....reformat so that seconds can be added from the beginning of the day
	dates <- as.POSIXct ( indates )
	
	#Julian day
	x1 = yday(indates)

	#in hours
	x2 <- suncalc ( x1, lat, lon )

	solarnoon <- (x2$sunrise + x2$sunset) / 2
		
	
	solarnoon_seconds <- round (solarnoon * 60 * 60, 0)
	solarnoon <- dates + solarnoon_seconds
	
	if (uniq) {solarnoon <- unique (solarnoon)}
	return (solarnoon)
	}























































































SunriseSunset <- function ( indates, lon, lat )
	{
	#timezone = timezone
	stopifnot ( class (indates) == 'Date' | 'POSIXct' %in% class (indates))
	
	
	#force indates to Date formate
	indates = as.Date( indates )
	dates <- as.POSIXct ( indates )
	
	x1 = yday(indates)

	#in hours
	x2 <- suncalc ( x1, lat, lon )

	solarnoon <- (x2$sunrise + x2$sunset) / 2

	sunrise_seconds <- round (x2$sunrise * 60 * 60, 0)
	sunrise <- dates + sunrise_seconds
	
	sunset_seconds <- round (x2$sunset * 60 * 60, 0)
	sunset <- dates + sunset_seconds
		
	solarnoon_seconds <- round (solarnoon * 60 * 60, 0)
	solarnoon <- dates + solarnoon_seconds
	
	x2$sunrise <- sunrise
	x2$sunset <- sunset
	x2$solarnoon <- solarnoon
	
	return (x2)
	}






























































SideRealTime <- function ( indates, lon_string )
	{
	#library (astroFns)


	stopifnot ( 'POSIXct' %in% class ( indates ))

	year = as.numeric ( format (indates, '%Y'))
	month = as.numeric ( format (indates, '%m'))
	day = as.numeric ( format (indates, '%d'))
	hour = as.numeric ( format (indates, '%H'))
	minute = as.numeric ( format (indates, '%M'))
	second = as.numeric ( format (indates, '%S'))

	ut2lst (yr = year, mo = month, dy = day, hr = hour, mi = minute, se = second, lon.obs = lon_string)
	#ut2ha (yr = year, mo = month, dy = day, hr = hour, mi = minute, se = second, lon.obs = lon_string)
	}







































TimeAfterSunrise <- function (indates, lon, lat, c_type = 'h', daytimeonly = TRUE )
	{
	stopifnot ( 'POSIXct' %in% class(indates))
	stopifnot ( c_type %in% c ('h','m','s'))
	
	x2 <- Sunrise (indates, lon, lat, uniq = FALSE)
	x3 <- Sunset (indates, lon, lat, uniq = FALSE)
	
	time_after_rise <- indates - x2
	time_after_set <- indates - x3

	if (daytimeonly)
		{
		time_after_rise[time_after_rise < 0] <- NA
		time_after_rise[time_after_set > 0] <- NA
		}

	if (c_type == 'h'){	time_after_rise = round (time_after_rise / (60 * 60), 2) }
	if (c_type == 'm'){	time_after_rise = round (time_after_rise / 60, 0) }
	#otherwise must be seconds

	return (time_after_rise)
	}

#TimeAfterSunrise (x1a, 1,54)




































TimeAfterSunset <- function (indates, lon, lat, c_type = 'h', night_time_only = TRUE )
	{
	stopifnot ( 'POSIXct' %in% class(indates))
	stopifnot ( c_type %in% c ('h','m','s'))
	
	x3 <- Sunset (indates, lon, lat, uniq = FALSE)
	
	time_after_set <- indates - x3

	if (night_time_only)
		{
		time_after_set[time_after_set < 0] <- NA
		
		}

	
	if (c_type == 'h'){	time_after_set = round (time_after_set / (60 * 60), 2) }
	if (c_type == 'm'){	time_after_set = round (time_after_set / 60, 0) }
	#otherwise must be seconds

	return (time_after_set)
	}

#TimeAfterSunset (x1a, 1,54)


















TimeAfterSolarNoon_arc1 <- function (indates, lon, lat, c_type = 'h', daytimeonly = TRUE )
	{
	stopifnot ( 'POSIXct' %in% class(indates))
	stopifnot ( c_type %in% c ('h','m','s'))
	
	x2 <- Sunrise (indates, lon, lat, uniq = FALSE)
	x3 <- Sunset (indates, lon, lat, uniq = FALSE)
	x4 <- SolarNoon (indates, lon, lat, uniq = FALSE)

	
	time_after_rise <- indates - x2
	time_after_set <- indates - x3
	time_after_noon <- indates - x4

	

	if (daytimeonly)
		{
		time_after_noon[time_after_rise < 0] <- NA
		time_after_noon[time_after_set > 0] <- NA
		}
	
	if (c_type == 'h'){	time_after_noon = round (time_after_noon / (60 * 60), 2) }
	if (c_type == 'm'){	time_after_noon = round (time_after_noon / 60, 0) }
	
	return (time_after_noon)
	}

#TimeAfterSunrise (x1a, 1,54)


































TimeAfterSolarNoon <- function (indates, lon, lat, c_type = 'h', daytimeonly = TRUE )
	{
	stopifnot ( 'POSIXct' %in% class(indates))
	stopifnot ( c_type %in% c ('h','m','s'))
	
	x2 <- Sunrise (indates, lon, lat, uniq = FALSE)
	x3 <- Sunset (indates, lon, lat, uniq = FALSE)
	x4 <- SolarNoon (indates, lon, lat, uniq = FALSE)

	
	time_after_rise <- as.numeric (indates - x2)
	time_after_set	 <- as.numeric  (indates - x3)
	time_after_noon <- as.numeric (indates - x4)

	

	if (daytimeonly)
		{
		time_after_noon[time_after_rise < 0] <- NA
		time_after_noon[time_after_set > 0] <- NA
		}
	
	if (c_type == 'h'){	time_after_noon = round (time_after_noon / (60 * 60), 2) }
	if (c_type == 'm'){	time_after_noon = round (time_after_noon / 60, 0) }
	
	return (time_after_noon)
	}

#TimeAfterSunrise (x1a, 1,54)






































TimeAfterSolarNoon01 <- function ( indata, n_round = 1 )
	{

	SolarNoon <- difftime (indata$Sunset, indata$Sunrise)
	SolarNoon <- (indata$Sunset - indata$Sunrise)/2 + indata$Sunrise
	
	TimeToNoon <- round(difftime(indata$date, SolarNoon, units = 'hours'), n_round)
	}







































SolarNoonTimeAfterMidnight <- function (indates, lon, lat, c_type = 'h')
	{
	stopifnot ( 'POSIXct' %in% class(indates))
	
	x4 <- SolarNoon (indates, lon, lat, uniq = FALSE)
	x4 <- SolarNoon (indates, lon, lat, uniq = TRUE)
	
	dates <- as.Date (indates)
	dates <- unique (as.Date (dates))
	#this sets to midnight
	dates <- as.POSIXct (dates)
	
	time_after_midnight <- x4 - dates
	
	if (c_type == 'h'){	time_after_midnight = round (time_after_midnight / (60 * 60), 2) }
	if (c_type == 'm'){	time_after_midnight = round (time_after_midnight / 60, 0) }
	
	return (time_after_midnight)
	}

#TimeAfterSunrise (x1a, 1,54)

