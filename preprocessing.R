rm(list = ls())

library(httr)
library(xml2)
library(jsonlite)
library(dplyr)
library(stringr)
library(magrittr)
library(naniar)
library(utils)
library(tidyr)

# Thu thap tu web TGDD-------------------------------------------------------------

tgdd <- data.frame()

html_to_dataset <- function(XPath){
  data <- xml_find_all(html_data, xpath = XPath)
  
  x <- xml_attrs(data)
  dataset <- as.data.frame(x)
  
  new_dataset <- data.frame(c = 1:length(x))
  new_dataset <- new_dataset[,FALSE]
  col_names <- row.names(dataset)
  
  for (i in 1:(nrow(dataset))){
    col_values <- xml_attr(data, attr = col_names[i])
    new_dataset <- cbind(new_dataset, as.data.frame(col_values))
  }
  new_dataset <- na.omit(new_dataset)
  names(new_dataset) <- col_names
  return(new_dataset)
}

for (page in 0:14){
  link <- paste('https://www.thegioididong.com/Category/FilterProductBox?c=44&o=9&pi=', page, sep = "")
  data_web <- POST(link)
  content_web <- content(data_web)
  html_data <- read_xml(content_web[['listproducts']], as_html = TRUE)
  
  XPath1 <- "//li//a[@class = 'main-contain']"
  XPath2 <- "//li"
  
  data1 <- html_to_dataset(XPath1)
  data2 <- html_to_dataset(XPath2)
  data <- cbind(data1, as.data.frame(data2$`data-price`))
  data <- rename(data, 'data-list-price' = 'data2$`data-price`')
  tgdd <- bind_rows(tgdd, data)
}

# Thu thap tu web Tiki-------------------------------------------------------------

tiki <- data.frame()

for (i in 1:9){
  link <- paste('https://tiki.vn/api/personalish/v1/blocks/listings?limit=48&include=advertisement&aggregations=2&trackity_id=2c7419a9-0d5f-5f53-10d2-ce6f23eddd3f&category=8095&page=', 
                i, '&urlKey=laptop', sep = "")
  api <- GET(link)
  content <- content(api)
  json_content <- toJSON(content)
  
  data <- fromJSON(json_content, flatten = TRUE)
  tiki <- bind_rows(tiki, as.data.frame(data['data']))
}

# Thu thap tu web Sendo------------------------------------------------------------

content <- read_json("Sendo.json")
json_content <- toJSON(content)

sendo <- fromJSON(json_content, flatten = TRUE)

# Thu thap tu web FPTshop----------------------------------------------------------

data <- read_json('FPTshop.json')
json_content <- toJSON(data)

fpt <- fromJSON(json_content, flatten = TRUE)

# Thu thap tu web CellphoneS-------------------------------------------------------

data <- read_json('CellphoneS.json')
json_content <- toJSON(data)

cellphones <- fromJSON(json_content, flatten = TRUE)

# Trich xuat ra cac cot can thiet-----------------------------------------------

tgdd <- tgdd %>% select(href, `data-name`, `data-price`, `data-brand`, `data-list-price`)
names(tgdd) <- c('url_path', 'Name', 'SalePrice', 'Brand', 'DefaultPrice')
tiki <- tiki %>% select(data.name, data.url_path, data.brand_name, data.price, data.list_price)
names(tiki) <- c('Name', 'url_path', 'Brand', 'SalePrice', 'DefaultPrice')
sendo <- sendo %>% select(name, category_path, default_price_max, sale_price_max)
names(sendo) <- c('Name', 'url_path', 'DefaultPrice', 'SalePrice')
fpt <- fpt %>% select(brandName, name, nameAscii, productVariant.price, productVariant.priceMarket)
names(fpt) <- c('Brand', 'Name', 'url_path', 'SalePrice', 'DefaultPrice')
cellphones <- cellphones %>% select(name, url, price, final_price, display_size, laptop_ram, hdd_sdd, laptop_cpu)
names(cellphones) <- c('Name', 'Link', 'DefaultPrice', 'SalePrice', 'ScreenSize', 'RAM', 'HardDisk', 'CPU')

# Mot so buoc tien xu ly--------------------------------------------------------

# Trich xuat thong tin tu cot laptop_name
extract_brand <- function(name){
  brand_name <- regex('lenovo|dell|hp|asus|msi|xiaomi|acer|apple|macbook|mac|microsoft|surface|fujitsu|ismart|gigabyte|avita|toshiba|chuwi|lg|masstel|sony|alienware|teclast|netbook|evoo|huawei|great asia|vostro|asprire|samsung|thinkpa+', ignore_case = TRUE)
  brand <- str_extract(name, brand_name)
  return(brand)
}
customize_brand <- function(brand){
  for(i in 1:length(brand)){
    if(!is.na(brand[i])){
      if(brand[i] %in% c(' Lenovo', 'Lenovo Thinkpad', 'thinkpa'))
        brand[i] <- 'Lenovo'
      else if(str_detect(brand[i], regex('mac', ignore_case = TRUE))) 
        brand[i] <- 'Apple'
      else if(str_detect(brand[i], regex('hp|msi|lg', ignore_case = TRUE))) 
        brand[i] <- toupper(brand[i])
      else if(str_detect(brand[i], regex('ismart', ignore_case = TRUE))) 
        brand[i] <- 'iSmart'
      else if(brand[i] == 'Asprire') 
        brand[i] <- 'Acer'
      else if(brand[i] == 'Surface')
        brand[i] <- 'Microsoft'
      else if(brand[i] %in% c('INSPIRON', 'Alienware', 'Vostro'))
        brand[i] <- 'Dell'
      else if(brand[i] == 'OEM')
        brand[i] <- 'HP'
      else 
        brand[i] <- str_to_title(brand[i])
    }
  }
  brand <- str_replace_na(brand)
}
extract_cpu <- function(name){
  cpu <- str_extract(name, regex('M1 Max|M1 Pro|M1|ryzen [3|5|7]|R[3|5|7]|i[3|5|7|9]|celeron|pentium', ignore_case = TRUE))
  return(cpu)
}
customize_cpu <- function(cpu){
  for(i in 1:length(cpu)){
    if(!is.na(cpu[i])){
      if(str_detect(cpu[i], regex('i[3|5|7|9]', ignore_case = TRUE)))
        cpu[i] <- paste("Intel Core", str_to_lower(cpu[i]))
      else if(cpu[i] == 'Celeron'||cpu[i] == 'Pentium')
        cpu[i] <- paste('Intel', cpu[i])
      else if(str_detect(cpu[i], 'R[3|5|7]'))
        cpu[i] <- paste('AMD Ryzen', str_extract(cpu[i], '3|5|7'))
      else if(str_detect(cpu[i], 'Ryzen [3|5|7]'))
        cpu[i] <- paste('AMD', cpu[i])
      else if(str_detect(cpu[i], regex('M1 Max|M1 Pro|M1', ignore_case = TRUE)))
        cpu[i] <- paste('Apple', str_to_title(cpu[i]))
    }
  }
  cpu <- str_replace_na(cpu)
}
extract_ram  <- function(name){
  ram <- str_extract(name, regex('16gb|32gb|64gb|4gb|8gb|16 gb|32 gb|64 gb|4 gb|8 gb|16g|32g|64g|4g|8g', ignore_case = TRUE))
  return(ram)
}
customize_ram <- function(ram){
  ram <- str_to_upper(ram)
  ram <- str_remove(ram, ' ')
  #ram <- str_remove(ram, 'GB|G')
  #ram <- str_replace(ram, 'FALSE', 'NA')
  for(i in 1:length(ram)){
    if(!is.na(ram[i]))
      if(ram[i] == '4G'||ram[i] == '8G'||ram[i] == '16G'||ram[i] == '32G'||ram[i] == '64G' )
        ram[i] <- paste(ram[i], 'B', sep = '')
  }
  ram <- str_replace_na(ram)
}
extract_hard_disk_size <- function(name){
  hard_disk_size <- str_extract(name, regex('128GB|256GB|512GB|1TB|2TB|64GB|128 GB|256 GB|512 GB|1 TB|2 TB|64 GB|128g|256g|512g|64g', ignore_case = TRUE))
  return(hard_disk_size)
}
customize_hard_disk_size <- function(hard_disk_size){
  hds <- hard_disk_size
  hds <- str_to_upper(hds)
  hds <- str_remove(hds, ' ')
  #hds <- str_remove(hds, 'TB|GB|G')
  #for(i in 1:length(hds)){
    #if(hds[i] %in% c('1', '2', '4', '8'))
      #hds[i] <- paste(hds[i], '000', sep = '')
  #}
  for(i in 1:length(hard_disk_size)){
    if(!is.na(hard_disk_size[i]))
      if(hard_disk_size[i] %in% c('128G', '256G', '512G'))
        hard_disk_size[i] <- paste(hard_disk_size[i], 'B', sep = '')
  }
  hds <- str_replace_na(hds)
  return(hds)
}
extract_hard_disk_type <- function(name){
  hard_disk_type <- str_extract(name, regex('HDD|SSD|eMMc', ignore_case = TRUE))
  return(hard_disk_type)
}
customize_hard_disk_type <- function(hard_disk_type){
  hdt <- hard_disk_type
  hdt <- str_remove(hdt, ' ')
  hdt <- str_to_upper(hdt)
  hdt <- str_replace_na(hdt)
  #hdt <- str_replace(hdt, 'FALSE', 'NA')
  for(i in 1:length(hdt))
    if(hdt[i] == 'EMMC')
      hdt[i] <- 'eMMc'
  return(hdt)
}
extract_screen_size <- function(name){
  screen <- str_extract(name, regex('[1-9]{2}["|\'|â]|[1-9]{2}.[0-9]["|\'|â]|[1-9]{2}inch|[1-9]{2}.[0-9]inch|[1-9]{2} inch|[1-9]{2}.[0-9] inch|[1-9]{2}-inch|[1-9]{2}.[0-9]-inch|[1-9]{2}in|[1-9]{2}.[0-9]in|[1-9]{2}fhd|[1-9]{2}.[0-9]fhd|[1-9]{2} fhd|[1-9]{2}.[0-9] fhd|[1-9]{2}hd|[1-9]{2}.[0-9]hd|[1-9]{2} hd|[1-9]{2}.[0-9] hd', ignore_case = TRUE))
  return(screen)
}
customize_screen_size <- function(screen){
  for(i in 1:length(screen)){
    if(!is.na(screen[i]))
      screen[i] <- str_extract(screen[i], '[1-9]{2}.[0-9]|[1-9]{2}')
  }
  screen <- str_replace(screen, ',', '.')
  
  screen <- str_replace_na(screen)
}

# TGDD

tgdd$Link <- paste('https://www.thegioididong.com', tgdd$url_path, sep = '')
tgdd$Brand <- customize_brand(tgdd$Brand)
tgdd$Website <- rep('TheGioiDiDong', nrow(tgdd))

# Tiki

tiki$Name <- unlist(tiki$Name)
tiki$url_path <- unlist(tiki$url_path)
tiki$Brand <- unlist(tiki$Brand)
tiki$SalePrice <- unlist(tiki$SalePrice)
tiki$DefaultPrice <- unlist(tiki$DefaultPrice)

tiki$Link <- paste('https://tiki.vn/', tiki$url_path, sep = '')
tiki$Brand <- customize_brand(tiki$Brand)
tiki <- tiki %>% filter(Brand != 'WIWU' || Brand != 'HI PC') %>%
                filter(str_detect(url_path, 'ban-phim|cap-ket-noi|decal|bo-dan|tui-chong-soc|dan-chong-trom|hyperdrive|mac-studio|sac') == FALSE)
tiki$Brand <- customize_brand(tiki$Brand)
tiki$CPU <- extract_cpu(tiki$Name)
tiki$CPU <- customize_cpu(tiki$CPU)
tiki$RAM <- extract_ram(tiki$Name)
tiki$RAM <- customize_ram(tiki$RAM)
tiki$HardDiskSize <- extract_hard_disk_size(tiki$Name)
tiki$HardDiskSize <- customize_hard_disk_size(tiki$HardDiskSize)
tiki$HardDiskType <- extract_hard_disk_type(tiki$Name)
tiki$HardDiskType <- customize_hard_disk_type(tiki$HardDiskType)
tiki$HardDisk <- paste(tiki$HardDiskType, tiki$HardDiskSize, sep = ': ')
tiki$ScreenSize <- extract_screen_size(tiki$Name)
tiki$ScreenSize <- customize_screen_size(tiki$ScreenSize)
tiki$Website <- rep('Tiki', nrow(tiki))

# Sendo

sendo$Link <- paste('sendo.vn/', sendo$url_path, sep = "")
sendo$Brand <- extract_brand(sendo$Name)
sendo$Brand <- customize_brand(sendo$Brand)
sendo <- sendo %>% filter(Brand != 'Unknown') %>%
  filter(str_detect(url_path, 'skin|tai-nghe|dong-ho|bao-chong-soc|mouse|man-hinh|balo|chuot|may-tinh-de-ban|pc') == FALSE)
sendo$CPU <- extract_cpu(sendo$Name)
sendo$CPU <- customize_cpu(sendo$CPU)
sendo$RAM <- extract_ram(sendo$Name)
sendo$RAM <- customize_ram(sendo$RAM)
sendo$HardDiskSize <- extract_hard_disk_size(sendo$Name)
sendo$HardDiskSize <- customize_hard_disk_size(sendo$HardDiskSize)
sendo$HardDiskType <- extract_hard_disk_type(sendo$Name)
sendo$HardDiskType <- customize_hard_disk_type(sendo$HardDiskType)
sendo$HardDisk <- paste(sendo$HardDiskType, sendo$HardDiskSize, sep = ': ')
sendo$ScreenSize <- extract_screen_size(sendo$Name)
sendo$ScreenSize <- customize_screen_size(sendo$ScreenSize)
sendo$Website <- rep('Sendo', nrow(sendo))

sendo$Name <- unlist(sendo$Name)
sendo$DefaultPrice <- unlist(sendo$DefaultPrice)
sendo$SalePrice <- unlist(sendo$SalePrice)

# FPT shop

fpt$Brand <- unlist(fpt$Brand)
fpt$Name <- unlist(fpt$Name)
fpt$url_path <- unlist(fpt$url_path)
fpt$SalePrice <- unlist(fpt$SalePrice)
fpt$DefaultPrice <- unlist(fpt$DefaultPrice)
fpt$Brand <- customize_brand(fpt$Brand)
fpt$Link <- paste('https://fptshop.com.vn/may-tinh-xach-tay/', fpt$url_path, sep = '')
fpt$Website <- rep('FPTshop', nrow(fpt))

# CellphoneS

cellphones <- cellphones %>% filter(DefaultPrice != '0.0000') %>%
                              filter(SalePrice != 0) %>% 
                              filter(str_detect(Link, 'apple-studio-display') == FALSE)
cellphones$Brand <- extract_brand(cellphones$Name)
cellphones$Brand <- customize_brand(cellphones$Brand)
cellphones$RAM <- customize_ram(cellphones$RAM)
cellphones$ScreenSize <- customize_screen_size(cellphones$ScreenSize)
cellphones$Website <- rep('CellphoneS', nrow(cellphones))
cellphones$Name <- unlist(cellphones$Name)
cellphones$CPU <- unlist(cellphones$CPU)
cellphones$DefaultPrice <- unlist(cellphones$DefaultPrice)
cellphones$SalePrice <- unlist(cellphones$SalePrice)
cellphones$Link <- unlist(cellphones$Link)

tgdd <- tgdd[c('Brand', 'Name', 'DefaultPrice', 'SalePrice', 'Link', 'Website')]
tiki <- tiki[c('Brand', 'Name', 'ScreenSize', 'CPU', 'RAM', 'HardDiskType', 'HardDiskSize', 'DefaultPrice', 'SalePrice', 'Link', 'Website')]
sendo <- sendo[c('Brand', 'Name', 'ScreenSize', 'CPU', 'RAM', 'HardDiskType', 'HardDiskSize', 'DefaultPrice', 'SalePrice', 'Link', 'Website')]
fpt <- fpt[c('Brand', 'Name', 'DefaultPrice', 'SalePrice', 'Link', 'Website')]
cellphones <- cellphones[c('Brand', 'Name', 'ScreenSize', 'CPU', 'RAM', 'HardDisk', 'DefaultPrice', 'SalePrice', 'Link', 'Website')]

# Xuat ra file csv de thuc hien dien cot con thieu tren excel-------------------

write.csv(tgdd, 'tgdd.csv', row.names = FALSE)
write.csv(tiki, 'tiki.csv', row.names = FALSE)
write.csv(sendo, 'sendo.csv', row.names = FALSE)
write.csv(fpt, 'fpt.csv', row.names = FALSE)
write.csv(cellphones, 'cellphones.csv', row.names = FALSE)