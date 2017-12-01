places_names <- c("AMES VETERANS MEMORIAL, IA",
                 "Ada Hayden Heritage Park, IA",
                 "Ames City Auditorium, IA",
                 "Ames/ISU Ice Arena, IA",
                 "Campustown Court, IA","City Hall,IA",
                 "Iowa State University, IA",
                 "Mcfarland clinic,IA",
                 "Planned Parenthood - Ames Health Center,IA",
                 "Story County Medical Center,IA")

places_Df <- data.frame(places_names)


#geocoding the places of interest
for(i in 1:length(places_names)){
  result <- geocode(places_names[i],
                    output = "latlona",
                    source = "google"
                    )
  places_Df$places_lon[i] <- as.numeric(result[1])
 places_Df$places_lat[i] <- as.numeric(result[2])
}

colnames(places_Df) <- c("places_names","lon","lat")
write.csv(places_Df, "coord_places.csv")

#geocoding the neighborhood in the data
loc <- read.csv("loc_coord.csv")
for(i in 1:length(loc)){
  i = i+1
  result <- geocode(loc$address[i],
                    output = "latlona",
                    source = "google"
                    )
  loc$lon[i] <- as.numeric(result[1])
  loc$lat[i] <- as.numeric(result[2])
}

write.csv(loc,"loc_coord.csv")

library(dplyr)
library(magrittr)

loc_coord%>%
  transmute(neighborhood,lon,lat)->x

places_Df <- read.csv("coord_places.csv")
colnames(places_Df) <- c("neighborhood","lon","lat")

#combine the neighborhood and the places of interest
y <-data.frame(rbind(places_Df,x))
y%>%filter(!is.na(lon))->y
# y <- transform(y, lat = as.numeric(lat))
# y%>%filter(!is.na(lon))->y
# lload Imap
library(Imap)

# create an empty list
dist_list <- list()

# iterate through data frame placing calculated distance next to place place names
for (i in 1:nrow(y)) {
  dist_list[[i]] <- gdist(lon.1 = y$lon[i], 
                          lat.1 = y$lat[i], 
                          lon.2 = y$lon, 
                          lat.2 = y$lat, 
                          units="miles")
  } 

# unlist results and convert to a "named" matrix format
dist_mat <- sapply(dist_list, unlist)
places_names <- c("AMES VETERANS MEMORIAL, IA",
                  "Ada Hayden Heritage Park, IA",
                  "Ames City Auditorium, IA",
                  "Ames/ISU Ice Arena, IA",
                  "Campustown Court, IA","City Hall,IA",
                  "Iowa State University, IA",
                  "Mcfarland clinic,IA",
                  "Planned Parenthood - Ames Health Center,IA",
                  "Story County Medical Center,IA",
                  'Bloomington Heights',
                  'Bluestem',
                  'Brookside',
                  'Clear Creek',
                  'College Creek',
                  'Crawford',
                  'Edwards',
                  'Gilbert',
                  'Iowa DOT and Rail',
                  'Meadow',
                  'Mitchell',
                  'North Ames',
                  'Northridege',
                  'Northpark Villa',
                  'Northridge Heights',
                  'Northwest Ames',
                  'Old Town',
                  'South & West of Iowa State University',
                  'Sawyer',
                  "Sawyer West",
                  "Somerset",
                  "Stone Brook",
                  "Timberland",
                  "Veenker")
colnames(dist_mat) <- places_names
rownames(dist_mat) <- places_names

# view results as matrix
dist_mat

remove <- rownames(dist_mat)[1:10]
dist_mat<-dist_mat[!rownames(dist_mat) %in% remove, ]
remove <- colnames(dist_mat)[11:34]
dist_mat <- dist_mat[,!colnames(dist_mat) %in% remove]
df <- data.frame(dist_mat)
library(scales)

data_1_%>%
  head(n = 1458)->k1

train%>%
  mutate(totalsqft = TotalBsmtSF+`1stFlrSF`+`2ndFlrSF`)%>%
  filter(totalsqft <= 7500)->t1

test%>%
  mutate(totalsqft = TotalBsmtSF+`1stFlrSF`+`2ndFlrSF`,
         SalePrice = lasso$SalePrice)->t2

data_1 <- rbind.data.frame(t1,t1)

write.csv(data_1,"combineddata.csv")


k2$price_per_sqfoot=k2$SalePrice/k2$totalsqft
k2$t=rescale(k2$price_per_sqfoot, to=c(-1,1))
k2$z=rescale(k2$OverallQual, to=c(-1,1))
k2$Neighborhood=as.factor(k2$Neighborhood)
k3=k2 %>% group_by(Neighborhood) %>% summarize(count=n(),price=mean(t),score=mean(z))

library(ggrepel)
############Perception Plots
ggplot(data=k3,
       aes(x=price,
           y=score,
           label=Neighborhood))+
  geom_point(aes(color=Neighborhood))+
  lims(x=c(-1,1),
       y=c(-1,1)) +
  theme_minimal() + 
  coord_fixed() + 
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) + 
  geom_text_repel(aes(label=Neighborhood),
                  size=2.5,
                  box.padding = 0.25)

k3=k2 %>% 
  group_by(Neighborhood,Condition1,Condition2) %>% summarize(count=n())

library(data.table)
df <-setDT(df, keep.rownames = TRUE)[]
df%>%
  rename(neighborhood = rn)%>%
  left_join(loc_coord, by="neighborhood")%>%
  rename(Neighborhood = code)->kk

combineddata%>%
  select(SalePrice, Neighborhood,totalsqft)%>%
  left_join(kk, by = "Neighborhood")->kk1

kk1$variable = rescale(kk1$SalePrice/kk1$totalsqft, to=c(-1,1))
kk1$key = rescale(kk1$Iowa.State.University..IA, to=c(-1,1))

kk1%>%
  ggplot(aes(x= variable ,
             y = key),
             label = Neighborhood)+
  geom_point(aes(color = Neighborhood))+
  lims(x=c(-1,1),
       y=c(-1,1)) +
  theme_minimal() + 
  coord_fixed() + 
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) + 
  geom_text_repel(aes(label=Neighborhood),
                  size=2.5,
                  box.padding = 0.25)
