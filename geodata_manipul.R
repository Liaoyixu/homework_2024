##HEADER --------------------------------------------------------------------
#
#Scrip name:   "geodata_manipol.R"
#Purpose:      "Write R code to set 2-km buffer along the le Doubs river and clip from the map"
#Author:       Yixu Liao
#Email:        lyx233@mail.ustc.edu.cn
#Date:         2024/4/22        
#
#SETUP ---------------------------------------------------------------------

###############################################################################
# 1、安装并加载所需的包
install.packages("terra")
install.packages("sf")
library(terra)
library(sf)

# 2、载入数据
doubs_dem <- terra::rast("Desktop//研一下学期课程/数据驱动的生态学研究方法/homework7/doubs_dem.tif")
doubs_river <- sf::st_read("Desktop/研一下学期课程/数据驱动的生态学研究方法/homework7/doubs_river.geojson")
doubs_points <- sf::st_read("Desktop/研一下学期课程/数据驱动的生态学研究方法/homework7/doubs_points.geojson")

# 3、转换投影坐标系
doubs_river_utm <- st_transform(doubs_river,32631) # 从原始坐标系转换到UTM投影坐标系（EPSG:32631）

# 4、建立缓冲区
doubs_river_buffer <- st_buffer(doubs_river_utm,dist = 2000) # 2000米缓冲区

# 5、地理数据可视化
plot(st_geometry(doubs_river_buffer),axes = TRUE)
library(ggplot2)
ggplot() + geom_sf(data = doubs_river_buffer) # 自行转换为地理坐标系

# 6、裁剪和掩膜处理
# 将原始的高程数据 (doubs_dem) 转换到相同的UTM坐标系
terra::crs(doubs_dem)  # 得到当前地理坐标系CRS
utm_crs <- "EPSG:32631"  # 设置新的地理坐标系CRS
doubs_dem_utm <- terra::project(doubs_dem,utm_crs) # 坐标系转换
terra::crs(doubs_dem_utm) # 再次确认坐标系
# 进行裁剪，仅保留河流缓冲区内的高程数据
doubs_dem_utm_cropped = crop(doubs_dem_utm,doubs_river_buffer)
# 使用 mask 函数进一步处理，确保在 doubs_river_buffer 范围内的数据被保留，而范围外的数据被设置为 NA（不可用）
doubs_dem_utm_masked = mask(doubs_dem_utm_cropped,doubs_river_buffer)
# 可视化处理后的数据
plot(doubs_dem_utm_masked,axes =TRUE)

# 7、使用QGIS算法处理
# 安装并加载QGIS处理库
install.packages("qgisprocess")
library(qgisprocess)
help("qgisprocess")

###############################################################################
# 搜索相关的QGIS算法 (使用除saga之外的算法)
qgis_algorithms()
qgis_search_algorithms("slope") |> # 关键词slope
  dplyr::select(provider_title,algorithm) |>
  head(2)
# 选择grass:r.slope.aspect计算坡度，通过函数查看算法所需要的具体参数
qgis_show_help("grass:r.slope.aspect")
qgis_list_input("grass:r.slope.aspect")
# 运行QGIS算法
topo_total = qgisprocess::qgis_run_algorithm(
  alg = "grass:r.watershed",
  elevation = doubs_dem_utm_masked,
  threshold = 1000,  # 这个阈值控制最小流域面积，需要根据实际情况调整
  accumulation = tempfile(fileext = ".tif"),  # 流累积量输出文件
  basin = tempfile(fileext = ".tif"),         # 流域输出文件
  drainage = tempfile(fileext = ".tif"),      # 流向图输出文件
  .quiet = FALSE)

slope_output = tempfile(fileext = ".tif")
qgis_run_algorithm("grass:r.slope.aspect", 
                   elevation = doubs_dem_utm_masked,
                   slope = slope_output,
                   aspect = tempfile(),  # 如果不需要方位角数据，也可以指定一个临时文件
                   format = "degrees",
                   precision = 0,
                   .quiet = FALSE)
# 搜索相关的QGIS算法
qgis_algorithms()
qgis_search_algorithms("watershed") |> # 关键词slope
  dplyr::select(provider_title,algorithm) |>
  head(2)
# 选择grass:r.watershed计算集水区，通过函数查看算法所需要的具体参数
qgis_show_help("grass:r.slope.aspect")
qgis_list_input("grass:r.slope.aspect")
# 运行QGIS算法
basin_output = tempfile(fileext = ".tif")
qgis_run_algorithm("grass:r.watershed",
                   elevation = doubs_dem_utm_masked,
                   accumulation = tempfile(),  # 流累积量，如果需要可以指定输出
                   drainage = basin_output,  # 输出流域
                   basin = basin_output,  # 输出集水区
                   threshold = 10000,  # 调整阈值以满足特定大小的集水区
                   .quiet = FALSE)
# 加载输出数据并计算面积
library(terra)
basins = rast(basin_output)  # 加载集水区栅格数据
area_per_basin = freq(basins, value = TRUE)  # 计算每个独立值的频率

###############################################################################

# 搜索相关QGIS算法
qgis_search_algorithms("wetness") |> dplyr::select(provider_title,algorithm)|>
  head(2)
# 使用算法计算面积和坡度数据
results = qgisprocess::qgis_run_algorithm(algorithm = "sagang:sagawetnessindex",
                                          DEM = doubs_dem_utm_masked,
                                          SLOPE_type=1,
                                          SLOPE=tempfile(fileext = ".sdat"),
                                          AREA = tempfile(fileext = ".sdat"),
                                          .quiet = TRUE)
# 提取并转换结果数据
results_select <- results[c("AREA","SLOPE")] |> 
  unlist() |>
  rast()   # 从算法输出中提取“AREA”和“SLOPE”字段，然后转换成栅格数据格式以便与其他地理数据整合和分析。
# 设置数据属性
names(results_select) = c("carea","cslope")
origin(results_select) = origin(doubs_dem_utm_masked)
results_char = c(doubs_dem_utm_masked,results_select)
# 将点数据转换为特定坐标系
doubs_points_utm <- sf::st_transform(doubs_points ,32631)
dim(doubs_points_utm) # 检查
# 数据提取和合并：extract 函数从合并后的数据集 topo_char 中提取与点数据 doubs_points_utm 对应的地形特征
results_env <- terra::extract(results_char,doubs_points_utm,ID = FALSE)
# 载入和增加环境数据
Doubs
water_env <- env
doubs_env = cbind(doubs_points_utm,results_env,water_env)

# 8、储存文件用于之后的工作
sf::st_write(doubs_env,("Desktop/doubs_env.shp"))