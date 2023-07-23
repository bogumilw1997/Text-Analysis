library(ggmap)
library(jsonlite)
library(geosphere, quietly = T, warn.conflicts = F)

register_google("AIzaSyCYgW_vyI25pkvWQDvAnz0I9GbtFsfX-Eg")

map <- get_googlemap(center = c(lon = 21.00, lat = 52.25), zoom = 11, maptype = "road")

um.waw.api <- "9c9a80e9-68d1-4f74-8d49-c241f3f5649f"
um.waw.url <- "https://api.um.warszawa.pl/api/action/busestrams_get/?resource_id=f2e5503e-927d-4ad3-9500-4ab9e55deb59&apikey="

url.api <- paste(um.waw.url, um.waw.api, "&type=2", sep = "")
trams <- fromJSON(url.api)

head(trams$result)
trams$result[trams$result$Lines == "11",]

df1 <- data.frame()
df2 <- data.frame()
df3 <- data.frame()

for (x in 1:30) {

  url.api <- paste(um.waw.url, um.waw.api, "&type=2", sep = "")
  trams <- fromJSON(url.api)

  df1 <- rbind(df1, trams$result[trams$result$VehicleNumber == "3609",])
  df2 <- rbind(df2, trams$result[trams$result$VehicleNumber == "3137",])
  df3 <- rbind(df3, trams$result[trams$result$VehicleNumber == "1383+1387",])

  print(x)
  Sys.sleep(60)
}

write.csv(df1, "df1.csv", row.names=FALSE)
write.csv(df2, "df2.csv", row.names=FALSE)
write.csv(df3, "df3.csv", row.names=FALSE)

df3 <- read.csv("df3.csv")
df3$Czas <- seq(1:nrow(df3))

ggmap(map) + 
  geom_point(data = df3, aes(x = Lon, y = Lat, color = Czas), size = 3) + 
  scale_color_gradient(low="blue", high="red")

D <- distm(df3[c(2, 5)], fun = distHaversine)/1000

distances <- c(0)

for (i in 1:29){
  distances <- append(distances,D[i,i+1])
}

df3$distances <- distances

time.diff <- c(0)

for (i in 2:30){
  time.diff <- append(time.diff,difftime(df3$Time[i], df3$Time[i-1], units = "secs"))
}

df3$Time.diff <- time.diff / (60 * 60) 

velocity <- c(0)
velocity <- append(velocity, df3$distances[-1]/df3$Time.diff[-1])

df3$Predkosc <- velocity

ggp <- ggplot(df3, aes(Czas, Predkosc)) + geom_line(color="black", linetype = "dotted", size=0.5) + 
  geom_point(colour = "red", size = 2) + stat_smooth(method = "lm",formula = y ~ poly(x, 4), se = FALSE) +
  labs(x = "Czas [min]", y = "Prędkość [km/h]") + ylim(0, 40)

ggp
