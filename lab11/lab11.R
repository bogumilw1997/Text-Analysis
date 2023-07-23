library(rtweet)
library(ggplot2)
token <- rtweet::create_token(
  app = "yourRgreat",
  consumer_key = "GwZ9eYp0gAgm1Zzk57MQHX2du",
  consumer_secret = "SOa0pINN9G1UQnhpx7DJzcuGvyq6nXCChGUzPvdLT6fuOiII1U",
  access_token = "414634084-ZbFxFAPE1a0kWSWBe3ffhyVuYSgOyxtGHglgGlfe",
  access_secret = "rtH4kpQBSm5NTC06s1mBmn5yZ7lMBzpkqNdmPmyt591jG")


tw <- get_timeline("cnn", n = 10000)
tw
ts_plot(tw, "5 hour", color = "gray") + geom_point(color = "blue")
library(tidytext)
library(dplyr)
emo <- as_tibble(read.csv("http://www.fizyka.pw.edu.pl/~julas/TEXT/lab/Ratings_Warriner_et_al.csv", stringsAsFactors = F))

emo <- emo %>% 
  select(word = Word, valence = V.Mean.Sum, arousal = A.Mean.Sum)

tw.data <- tw %>% transmute(id = row_number(), text = text) %>% unnest_tokens(word, text)

tw.sent <- tw.data %>% 
  inner_join(emo) %>%
  group_by(id) %>%
  summarise(valence = mean(valence))
tw$valence[tw.sent$id] <- tw.sent$valence
ggplot(tw, aes(x = created_at, y = valence)) + 
  geom_line(alpha=0.25, color = "red") + 
  geom_smooth(method = "loess", span = 0.01)
library(ggmap)
register_google("AIzaSyBKwXz1qbfECp8dr_POylJu2xePjcVBjnM")

map <- get_googlemap(center = c(lon = 21.00, lat = 52.25), zoom = 11, maptype = "road")
ggmap(map)
stops <- readLines("http://www.if.pw.edu.pl/~julas/TEXT/lab/stops.txt")
head(stops)
get.coor <- function(line) {
  
  p <- regexpr("[0-9]{6}", line)
  y <- regexpr("Y= 52.[0-9]{5,6}", line)
  x <- regexpr("X= 2[0-1].[0-9]{5,6}", line)
  
  p <- substr(line, p[1], p[1] + attr(p, "match.length") - 1)
  y <- substr(line, y[1] + 3, y[1] + attr(y, "match.length") - 1)
  x <- substr(line, x[1] + 3, x[1] + attr(x, "match.length") - 1)
  
  data.frame(stop_id = as.numeric(p), long = as.numeric(y), lat = as.numeric(x))
  
}
stops <- get.coor(stops[grep("Y= ", stops)])

head(stops)
map <- get_googlemap(center = c(lon = 21.00, lat = 52.25), zoom = 11, maptype = "road", color = "bw", language = "PL")
library(jsonlite)
um.waw.api <- "9c9a80e9-68d1-4f74-8d49-c241f3f5649f"
um.waw.url <- "https://api.um.warszawa.pl/api/action/busestrams_get/?resource_id=f2e5503e-927d-4ad3-9500-4ab9e55deb59&apikey="

url.api <- paste(um.waw.url, um.waw.api, "&type=2", sep = "")
trams <- fromJSON(url.api)

head(trams$result)
map <- get_googlemap(center = c(lon = 21.00, lat = 52.25), zoom = 12, maptype = "road", color="bw", language = "PL")
ggmap(map) + 
  geom_point(data = trams$result, aes(x = Lon, y = Lat))


trams$result$Lines <- as.numeric(gsub(" ", "", trams$result$Lines))

ggmap(map) + 
  geom_point(data = trams$result, aes(x = Lon, y = Lat, color = as.factor(Lines)), size = 4) +
  geom_text(data = trams$result, aes(x = Lon, y = Lat, label = Lines), size = 3) +
  scale_color_discrete(guide = FALSE)

airly.api = "0oRd9wGrqtnhN91H1UaAhPwKjRhGrmxy"
airly.url1 <- "https://airapi.airly.eu/v2/installations/nearest?lat=52.25&lng=21.00&maxDistanceKM=30&maxResults=45&apikey="

query <- paste(airly.url1, airly.api, sep = "")

czujniki <- fromJSON(query)
head(czujniki)

get.air.mes <- function(id, api.key) {
  
  query <- sprintf("https://airapi.airly.eu/v2/measurements/installation?installationId=%d&apikey=%s", id, api.key)
  print(id)
  #Sys.sleep(1)
  fromJSON(query)
  
}

pomiary <- lapply(czujniki$id, get.air.mes, api.key = airly.api)

indexes <- sapply(1:length(pomiary), function(i) pomiary[[i]]$current$indexes$value)

data <- cbind(czujniki$location, indexes)

head(data)
ggmap(map) + geom_point(data = data, aes(x = longitude, y = latitude, color = indexes), size = 4) +
  scale_color_gradient2("AQI", low = "green", high = "violet", mid = "red", midpoint = 85)

library(corrplot)
historia <- sapply(1:length(pomiary), function(i) sapply(pomiary[[i]]$history$indexes, function(x) x$value))
C <- cor(historia)
corrplot(C, type = "upper", na.label = "-", diag = F)
library(geosphere, quietly = T, warn.conflicts = F)
library(fields, quietly = T, warn.conflicts = F)

library(Hmisc, quietly = T, warn.conflicts = F)

D <- distm(data[c(2, 1)], fun = distHaversine)/1000

d <- stats.bin(D[upper.tri(D)], C[upper.tri(C)])

x <- d$centers
y <- d$stats[2,]
yerr <- d$stats[3] / sqrt(d$stats[1,])

par(mfrow = c(1,2))
plot(D[upper.tri(D)], C[upper.tri(C)], pch = 19, cex = 0.5, col = "gray", tcl = 0.25, xlab = "odelglosc [km]", ylab = "korelacja")
errbar(x, y, y + yerr, y - yerr, add = T, col = "blue", errbar.col = "blue")
errbar(x, y, y + yerr, y - yerr, col = "blue", errbar.col = "blue", tcl = 0.25, xlab = "odelglosc [km]", ylab = "korelacja")
par(mfrow = c(1,1))
