library(sf)
library(dplyr)
library(readr)
library(raster)
source("./code/oil_prices.R")

price.lvls <- c("Xlow", "low", "med", "high")


template <- raster("./data/Template.tif")
intangibles <- raster("../Zonation_Project/input_layers/Mask_intang_clip.tif")
BlockInfo <- read_csv("./data/OilBlockInfo.csv",
                      locale=locale(encoding = 'ISO-8859-1'))


for(price.lvl in price.lvls){
  OilBlocks.polys <- read_sf("./data/OilBlocks2.shp")
  BlockInfo <- read_csv("./data/OilBlockInfo.csv",
                        locale=locale(encoding = 'ISO-8859-1'))
  if(price.lvl=="high"){
    price <- p2
  }else if(price.lvl=="med"){
    price <- p1
  }else if(price.lvl=="low"){
    price <- 39
  }else if(price.lvl=="Xlow"){
    price <- p3
  }else(
    break
  )
  
  
  
  # Price / cost / volume parameters
  southern_vol <- 520.64271772693  # Production volume per km2 of southern blocks
  OilPrice <- price
  ProdCost <- 25 # Production cost per barrel
  ProdCost.south <- 28
  ProdCost.comp <- 38
  
  southern.reserves <- 369000000 # Reserve barrels in southern blocks
  # Profit margin ~15% was assumed to calculate the net income per unit area from
  # Naidoo and Iwamura (2007) and corrected for 2016 prices
  AgriValue <- 2000  # $/km^2
  
  
  #
  OilBlocks.polys <-
    OilBlocks.polys %>%
    select(2,3,4) %>%
    arrange(Oil_bloc_2) %>%
    rename(Block_No=Oil_bloc_2, Block_name=Sheet1__Bl, Operator=Sheet1__Op) %>%
    mutate(Area_km2=st_area(geometry)) %>%
    mutate(Area_km2=units::set_units(Area_km2, km^2)) %>%
    select(1,5,4)
  
  #st_write(OilBlocks.polys, "./data/OilBlocks_cln.shp", delete_layer = TRUE)
  #st_read("./data/OilBlocks_cln.shp")
  
  BlockInfo <-
    BlockInfo %>%
    rename(Block_No='Block No.', Block_name='Block name',
           Block_area_km2='Block area (km2)',
           Barrels_2015='Barrels produced 2015',
           Barrels_2016='Barrels produced 2016',
           Inferred_barrels='Inferred annual barrels') %>%
    mutate(Category=as.factor(Category)) %>%
    select(-5)
  
  #setdiff(OilBlocks.polys$Operator,BlockInfo$Operator)
  #setdiff(BlockInfo$Operator, OilBlocks.polys$Operator)
  
  #setdiff(OilBlocks.polys$Block_name,BlockInfo$Block_name)
  #setdiff(BlockInfo$Block_name, OilBlocks.polys$Block_name)
  
  blocks.poly <- st_as_sf(full_join(BlockInfo, OilBlocks.polys, by="Block_No"))
  
  
  #st_write(select(blocks.poly, -5:-7), "./data/OilBlocks_cln.shp", delete_layer = TRUE)
  
  # For the anticipated production density of the Southern blocks
  southern.area <-
    blocks.poly %>%
    group_by(Category) %>%
    summarise(sum(Area_km2)) %>%
    filter(Category=='Southern unexploited')
  
  south.AnnualProdDens <- (southern.reserves/20)/southern.area$`sum(Area_km2)`
  
  
  # 
  blocks.poly <-
    blocks.poly %>%
    mutate(Production=pmax(Barrels_2016, Inferred_barrels, na.rm = TRUE)) %>%
    mutate(Revenue=OilPrice*Production) %>%
    mutate(Cost=if_else(Category=="Active" | Category=="Growing", ProdCost*Production,
                        if_else(Category=="Southern unexploited", ProdCost.south*Production,
                                if_else(Category=="Complex reserves", ProdCost.comp*Production, 0)))) %>%
    mutate(Profit=Revenue-Cost) %>%
    mutate(ActiveOil=ifelse(Profit > (AgriValue*as.numeric(Area_km2)), 1, 0)) %>%
    mutate(Profit=ifelse(Profit > (AgriValue*as.numeric(Area_km2)), Profit, (AgriValue*as.numeric(Area_km2)))) %>%
    select(1:8, 10:14, geometry)
  
  
  
  
  areaRas <- area(template)
  areaRas[is.na(template)] <- NA
  #plot(areaRas)
  
  # Compare area of raster pixels and shape file - how should we define pixel size
  #Total Area / Number of pixels (for pixel area)
  #pxl.area <- 0.928574414031877^2 #km2
  #pxl.area <- cellStats(areaRas, sum)/cellStats(template, sum)
  pxl.area <- sum(st_area(blocks.poly))/cellStats(template, sum)
  #pxl.area <- sum(st_area(blocks.proj))/1e6/cellStats(template, sum)
  
  blocks.poly <- 
    blocks.poly %>%
    mutate(pixels=Area_km2/pxl.area) %>%
    mutate(dlr_pxl=as.numeric(Profit/pixels)) %>%
    mutate(dlr_km2=as.numeric(Profit/Area_km2)) %>%
    select(-geometry)
  
  # Transform into prioritisation CRS
  blocks.proj <- st_transform(blocks.poly, crs(template))
  ras <- rasterize(blocks.proj, template, 'dlr_pxl')
  ras.oil <- rasterize(blocks.proj, template, 'ActiveOil')
  
  # Some missing pixels? Make them agricultural!
  ras.missing <- (template - !is.na(ras))
  
  ras2 <- ras.missing
  ras2[ras2==1] <- cellStats(ras, min)
  
  template0 <- template - template
  
  ras3 <- ras2 + ras
  all(dim(ras3)==dim(template))
  #plot(ras3)
  
  ras.oil <- template0 + ras.oil
  ras.agri <- -ras.oil + 1
  
  total.profit <- cellStats(ras3, sum)
  oil.profit <- cellStats(ras.oil * ras3, sum)
  agri.prfit <- cellStats(ras.agri * ras3, sum)
  
  plot(blocks.poly[16], main="Land value (USD km^-2 yr^-1)")
  # One column for oil profit and one for agricultural - choose largest
  dir.create("cost_layers", showWarnings = FALSE)
  writeRaster(ras3, paste0("./cost_layers/","LandVal_price", OilPrice, ".tif"), overwrite=TRUE)
  writeRaster(ras.oil, paste0("../Zonation_Project/input_layers/activeOil_", price.lvl, ".tif"), overwrite=TRUE)
  
}
rm(BlockInfo, OilBlocks.polys)

