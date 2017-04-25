library(XML)
library(ggplot2)
library(ggmap)
library(RSocrata)

url_trees <- 'https://data.cityofnewyork.us/resource/nwxe-4ae8.csv'
url_zips <- 'http://zipatlas.com/us/ny/brooklyn/zip-code-comparison/median-household-income.htm'

trees <-read.socrata(url_trees)
trees <- subset(trees, boroname == 'Brooklyn')
trees$tree_dbh <- as.numeric(trees$tree_dbh)

neighborhoods = readHTMLTable(url_zips, header=T, which=1, stringsAsFactors=F)
neighborhoods = neighborhoods[complete.cases(neighborhoods), c(2,5,6)]
neighborhoods = neighborhoods[-1, ]
names(neighborhoods) <- c("zipcode", "population", "income")
neighborhoods$population <- as.numeric(gsub("[$,]", "", neighborhoods$population))
neighborhoods$income <- as.numeric(gsub("[$,]", "", neighborhoods$income))

extras <- do.call(data.frame, aggregate(tree_dbh ~ zipcode, trees, 
                                        FUN=function(x) c(mn = mean(x), count = length(x))))
neighborhoods <- merge(neighborhoods, extras, by="zipcode")
neighborhoods$trees_per_capita <- neighborhoods$tree_dbh.count/neighborhoods$population

ggplot(neighborhoods, 
       aes(x=income, y=tree_dbh.mn, size = trees_per_capita, label=neighborhoods$zipcode)) +
  geom_point(color="green") + geom_smooth(method='lm',formula=y~x, show.legend =F) +
  geom_text(size=4, nudge_x = 1300, nudge_y = c(0.1, -0.1, -0.1)) +
  scale_size_continuous(range=c(0,10)) +
  labs(x="Median Household Income", y="Tree Diameter") +
  theme(title=element_text(face="bold.italic", color="black"))

zoom <- subset(trees, zipcode %in% c(11239, 11206, 11212, 11224, 11221, 
                                     11201, 11215, 11217, 11231, 11234))

zoom$hood <- as.factor(ifelse(zoom$zipcode %in% c(11201, 11215, 11217, 11231, 11234), 1,0))

map <- get_map(location = c(lon = -73.95, lat = 40.64), zoom = 12,
               maptype = "satellite", source = "google")
ggmap(map) + geom_point(data=zoom, aes(x = longitude, y = latitude, col = hood),
                        size = 0.5, shape = 16, alpha = 0.1, show.legend = F)


zoom$cherry <- grepl("cherry", zoom$spc_common)
zoom$dead <-zoom$status == "Dead"
table(zoom$hood, zoom$dead)
table(zoom$hood, zoom$cherry)
table(zoom$hood, zoom$brch_shoe)
