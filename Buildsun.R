
Buildsun <- function(n_ver)
	{
	
	packagename <- "sun"
	
	#try (library(roxygen2))
	#try (library(tools))
	#try (library(inlinedocs))
	#try (library(devtools))


	dirtop <- "/home/sjparker/sp/"

	dirpackagetop     <-  paste( dirtop, "Rpackage/", sep = "")
	dirpackage <- paste(dirpackagetop, packagename, sep = "")


	setwd ( dirpackagetop)


	c_check <-  paste ( "R CMD check      ", dirpackage )
	system (c_check)
 
	c_build <-  paste ( "R CMD build --resave-data      ", dirpackage )
	system (c_build)

	install_string <- paste ( 'install.packages ( paste ( dirdmp, "/", packagename, "_", n_ver, ".tar.gz", sep = ""), type = "source" )')

	return (install_string)
	}


#x1 <- Buildsun (1.0)
#install.packages ( 'sun_1.0.tar.gz', type = "source" )
