#HEADER --------------------------------------------------------------------
#
#Script name:  "pak_info.R"
#Purpose:      access information about the tidyverse package
#Author:       Yixu Liao
#Email:        1298283611@qq.com
#Date:         2024/3/13
#
#SETUP ---------------------------------------------------------------------

#install tidyverse packages

install.packages("tidyverse") #install packages

#Loading tidyverse package

library(tidyverse) #attaching core tidyverse packages 

#help

help(package="tidyverse")

#access information about the tidyverse package

is("package:tidyverse") #viewing the functions and datasets in the package
apropos("tidyverse") #showing the components of the tidyverse

vignette("tidyverse") #find the tidyverse tutorial library

browseVignettes(package = "tidyverse") #view the tidyverse package in a web page
demo(package = "tidyverse")

#---------------------------------------------------------------------------