#install the gapminder data using
install.packages("gapminder")
# load the gapminder data set
library(gapminder)
data(gapminder)
head(gapminder)
#Create a vector x of the life expectancies of each country for the year 1952.
x <- gapminder[ gapminder$year == 1952, ]
hist(x)
#Plot a histogram of these life expectancies to see the spread of the different countries.
plot(x$lifeExp, x$year)
hist(x$lifeExp)
#Probability Distributions Exercises #1
#What is the proportion of countries in 1952 that have a life expectancy less than or equal to 40?
mean(x$lifeExp <= 40)
#sapply() on a custom function
prop = function(q) {
  mean(gapminder[gapminder$year == 1952, ]$lifeExp <= q)
}
prop(40)
#build a range of qs that we can apply the function
qs = seq(from = min(x$lifeExp), to = max(x$lifeExp), length = 20)
#Now we can use sapply() to apply the prop function to each element of qs
props = sapply(qs, prop)
#Take a look at props, either by printing to the console, or by plotting it over qs:
plot(qs, props)
ecdf_plot <- ecdf(gapminder$lifeExp)
plot(ecdf_plot)
