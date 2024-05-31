##HEADER --------------------------------------------------------------------
#
#Scrip name:   "r_database.R"
#Purpose:      "Upload data from the ade4 package's built-in dataset, Doubs, to a PostgreSQL or SQLite schema."
#Author:       Yixu Liao
#Email:        lyx233@mail.ustc.edu.cn
#Date:         2024/4/12       
#
#SETUP ---------------------------------------------------------------------

################################################################################
# 在RStudio中连接PostgreSQL数据库

# 下载和加载所需包
install.packages("DBI")
install.packages("RPostgreSQL")
install.packages("odbc")
install.packages("dplyr")
library(DBI)
library(RPostgreSQL)
library(RODBC)
library(odbc)
library(dplyr)
library(dbplyr)

# 设置数据库连接参数
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv,
                 dbname = "postgres",
                 host = "localhost",
                 port = 5432,
                 user = "liaoyi",
                 password = "200051")

################################################################################
# 将 ade4 包中的 doubs 数据集上传到 PostgreSQL 数据库

# 加载所需包
library(reticulate)
library(RPostgreSQL)
library(ade4)

# 载入数据
data("doubs")
doubs
# 连接到 PostgreSQL 数据库
drv <- dbDriver("PostgreSQL")
con <- dbConnect(drv, dbname="postgres", host="localhost", 
                 port=5432, user="liaoyi", password="200051")
con
# 在数据库中发送 SQL 命令来创建一个新的模式。
dbSendQuery(con, "CREATE SCHEMA IF NOT EXISTS my_schema")

# 将数据写入数据库
tables <- list(doubs_env = doubs$env, doubs_fish = doubs$fish,
               doubs_xy = doubs$xy, doubs_species = doubs$species)
lapply(names(tables), function(tbl) {
  dbWriteTable(con, paste("my_schema", tbl, sep = "."), tables[[tbl]], overwrite = TRUE, row.names = FALSE)
})

# 查询数据并存储在 R 中的数据框中
query_result <- dbGetQuery(con, "SELECT * FROM my_schema.doubs_env")
print(query_result)
# 断开连接
dbDisconnect(con)
