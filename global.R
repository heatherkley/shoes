library(ggplot2)
library(shiny)
library(dplyr)
library(xtable)
library(shinydashboard)
library(DT)

shoes = read.csv("shoes.csv", stringsAsFactors = F)   # 'data.frame' structure
rownames(shoes) = NULL
############################################################################################
#Remove items that aren't shoes,,,,,, takes the dataset from 30k to 10.5k
nonshoes = c('pants','jeans', 'jacket','cardigan', 'dress','shirt','romper','suit','pedicure', 'bra','underwire','wirefree','bikini','swim','lingerie','thong','pantyhose','hairpin',
             'swimsuit','swimwear','scarf','glove','leggings','eyewear','Sunglasses',"Costume",
             'dress','blazer','men\'s', 'skirt','coat','top','socks','Doll','sleepwear','makeup',
             'bracelet','necklace','jewelry','diamond','band','ring','watch','Parka','lens', 'sweater', 'lotion', 'tunic', 'Messenger',
             'Halloween', 'robe', 'Blouse', 'silk', 'Eyeglasses')

shoes = shoes %>% 
  filter(!brand %in% c('TwoBirch','TBJE Wedding Bands','Yours Clothing','Brioni','GEMaffair',
                       'TheBeJeweledEgg Rings','Studs Galore','Techno-Marine','LUXURMAN',
                       'TBJE Slide Pendants','Peacock Diamonds','JUNGHANS','Ddi'), 
         !is.na(brand))%>% 
  filter(!grepl(paste(nonshoes, collapse = "|"),name, ignore.case = T))  # 10592 obs. of 50 var

############################################################################################
# convert price function
convertPrice = function(currency, amount){
  ifelse(currency=='AUD', amount * 0.75,
         ifelse(currency=='CAD', amount * 0.73,
                ifelse(currency=='EUR', amount * 1.1,
                       ifelse(currency=='GPB', amount * 1.3, 
                              amount))))}

#Convert existing column 'pricesAmountMax' & 'Min' to be numeric
shoes$pricesAmountMax = as.numeric(as.character(shoes$pricesAmountMax))
shoes$pricesAmountMin = as.numeric(as.character(shoes$pricesAmountMin))

#Convert existing column 'pricesAmountMax' & 'Min' to USD using convertPrice() function
shoes$pricesAmountMin = convertPrice(shoes$prices.currency, shoes$pricesAmountMin)
shoes$pricesAmountMax = convertPrice(shoes$prices.currency, shoes$pricesAmountMax)

############################################################################################
# merchants are selling the same product so the ID field is duplicated

shoes = shoes %>% 
  group_by(id) %>% 
  mutate(Count= n(), 
         brand = first(brand),
         ShoePriceMax = mean(pricesAmountMax, na.rm=TRUE, 2),
         ShoePriceMin = mean(pricesAmountMin, na.rm=TRUE, 2),
         ShoePrice = round((ShoePriceMax + ShoePriceMin) / 2, 2)) %>%
  arrange(desc(ShoePrice)) %>% 
  filter(!is.nan(ShoePrice))         #  returns 10470 obs of 12 var

############################################################################################
# CLASSIFY SHOES BY TYPE

patternreplace = function(x, patterns, replacements = patterns, fill = NA, ...)
{ stopifnot(length(patterns) == length(replacements))
  ans = rep_len(as.character(fill), length(x))    
  empty = seq_along(x)
  
  for(i in seq_along(patterns)) {
    greps = grepl(patterns[[i]], x[empty], ... , ignore.case = T)
    ans[empty[greps]] = replacements[[i]]  
    empty = empty[!greps]}
  return(ans)}


shoes = shoes %>% 
  mutate(type=patternreplace(name, 
                             c('flat','mocassin','moccasin','loafer', 'Espadrille','boot',        # 1 
                               'heel','Mary Janes', 'gladiator','stiletto','wedge','pump',        # 2
                               'sneaker','running','trainer','sandal','slipper','mule',           # 3
                               'clog', 'walking', 'canvas', 'slip', 'training', 'skate',          # 4
                               'flip', 'tennis', 'basketball', 'athletic', 'ballet', 'platform',  # 5
                               'oxford', 'converse', 'vans', 'cleats', 'boat', 'adidas',          # 6
                               'golf','asics', 'UGG', 'Nike', 'New Balance'),                     # 7
                             
                             c('flats','flats','flats','flats','flats','boots',                       # 1
                               'heels','heels','sandals','heels','heels','heels',                # 2
                               'sneakers','sneakers','sneakers','sandals','sandals','sandals',        # 3
                               'sandals', 'sneakers', 'sneakers', 'flats', 'sneakers', 'sneakers',    # 4
                               'sandals', 'sneakers', 'sneakers','sneakers', 'flats', 'heels',        # 5
                               'flats', 'sneakers', 'sneakers', 'sneakers', 'sneakers','sneakers',    # 6
                               'sneakers', 'sneakers', 'boots', 'sneakers', 'sneakers')))             # 7


############################################################################################

shoes$brand = ifelse(shoes$brand=='PUMA', 'Puma', as.character(shoes$brand))
shoes$brand = ifelse(shoes$brand=='Lauren Ralph Lauren', 'Ralph Lauren', as.character(shoes$brand))
shoes$brand = ifelse(shoes$brand=='MICHAEL Michael Kors', 'Michael Kors', as.character(shoes$brand))
shoes$brand = ifelse(shoes$brand %in% c('Pleaser Shoes', 'PleaserUSA'), 'Pleaser', as.character(shoes$brand))
shoes$prices.merchant = ifelse(shoes$prices.merchant== "Costumes4less.com - Walmart.com", "Costumes4less.com", as.character(shoes$prices.merchant))
shoes$prices.merchant = ifelse(shoes$prices.merchant== "Miles Kimball Company - Walmart.com", "Miles Kimball Company", as.character(shoes$prices.merchant))
shoes$prices.merchant = ifelse(shoes$prices.merchant== "Costumes4less.com - Walmart.com", "Costumes4less.com", as.character(shoes$prices.merchant))
shoes$prices.merchant = ifelse(shoes$prices.merchant== "Tasharina Corp - Walmart.com", "Tasharina Corp", as.character(shoes$prices.merchant))
shoes$prices.merchant = ifelse(shoes$prices.merchant== "Tags Weekly - Walmart.com", "Tags Weekly", as.character(shoes$prices.merchant))
shoes$prices.merchant = ifelse(shoes$prices.merchant== "BHFO, Inc. - Walmart.com", "BHFO, Inc.", as.character(shoes$prices.merchant))
shoes$prices.merchant = ifelse(shoes$prices.merchant== "ShoeFabs - Walmart.com", "ShoeFabs", as.character(shoes$prices.merchant))
shoes$prices.merchant = ifelse(shoes$prices.merchant== "Brands R US - Walmart.com", "Brands R US", as.character(shoes$prices.merchant))


############################################################################################
# remove rows with empty merchant and type 
shoes = shoes %>% 
  filter(!is.na(type)) %>%
  filter(prices.merchant != "")

# shoes = shoes %>% 
#   mutate(prices.merchant=patternreplace(prices.merchant,
#                              c(' - Ebay.com'),
#                              c('Ebay')))

############################################################################################
# add priceCategory column

shoes = shoes %>%
  group_by(brand) %>%
  mutate(priceCategory= ifelse(ShoePrice > 150, 'Luxury',
                               ifelse(ShoePrice <= 150 & ShoePrice > 40, 'MidPriced',
                                      'Cheap')))
#shoes = shoes %>%
#  factor(., shoes$priceCategory, levels = c("Cheap", "MidPriced", "Luxury))

############################################################################################
# FIX COLUMNS 

shoes = shoes[,c("brand", "type", "name", "Count", "ShoePriceMax", "ShoePriceMin", "ShoePrice", "priceCategory", "prices.isSale", "prices.merchant", "prices.offer", "colors")] 

shoes = shoes[!(is.na(shoes$brand) | shoes$brand==""), ]    # 10,378 obs of 13 variables 

# ADD BrandMaxPrice column
shoes$brandMaxPrice = ifelse(shoes$ShoePriceMax==max(shoes$ShoePriceMax),"Yes","No") # 10,378 obs of 14 variables

############################################################################################
# ADD BRANDQUANTITY & BRANDQUANTITYRATIO COLUMNS to shoes table

options(digits = 3)

#brand discount ratio
shoes = shoes %>% 
  group_by(brand) %>% 
  mutate(BrandQuantity = n()) %>% 
  mutate(BrandDiscountQuantity= sum(prices.isSale== "TRUE")) %>%
  mutate(BrandDiscountRatio = BrandDiscountQuantity / BrandQuantity*100)# %>%
 # mutate(BrandAvgPrice = mean(ShoePrice))

# type discount ratio
shoes = shoes %>% 
  group_by(type) %>% 
  mutate(TypeQuantity = n()) %>%
  mutate(TypeDiscountQuantity= sum(prices.isSale== "TRUE")) %>%
  mutate(TypeDiscountRatio = TypeDiscountQuantity / TypeQuantity*100) 

#brand type discount ratio
shoes = shoes %>% 
  group_by(brand, type) %>% 
  mutate(BrandTypeQuantity = n())  %>%     # 7681 obs of 17 vars
  mutate(BrandTypeDiscountQuantity = sum(prices.isSale== "TRUE")) %>%
  mutate(BrandTypeDiscountRatio = BrandTypeDiscountQuantity / BrandTypeQuantity*100) %>%
  mutate(BrandTypeMaxPrice = max(ShoePrice))

# merchant discount ratio
shoes = shoes %>%
  group_by(prices.merchant) %>%
  mutate(MerchantQuantity = n()) %>%
  mutate(MerchantDiscountQuantity= sum(prices.isSale == "TRUE")) %>%
  mutate(MerchantDiscountRatio = MerchantDiscountQuantity / MerchantQuantity*100)

############################################################################################
brands = data.frame(sort(table(shoes$brand), decreasing = TRUE))

# top 25 brands
brands20 = brands[2:21,]
top20brands = shoes[which(shoes$brand %in% brands20$Var1),]

# top 80 brands
brands80 = brands[2:81,]                                        # 80 obs of 2 var
top80brands = shoes[which(shoes$brand %in% brands80$Var1),]

# top 150 brands
brands150 = brands[2:151,]
top150brands = shoes[which(shoes$brand %in% brands150$Var1),]

# top 200 brands
brands200 = brands[2:201,]
top200brands = shoes[which(shoes$brand %in% brands200$Var1),]

# top 20 merchants
merchants = data.frame(sort(table(shoes$prices.merchant), decreasing = TRUE))
merchants20 = merchants[2:21,]
top20merchants = shoes[which(shoes$prices.merchant %in% merchants20$Var1),]


LuxuryShoes = top150brands %>% 
  group_by(brand) %>%
  filter(priceCategory == 'Luxury')


############################################################################################

