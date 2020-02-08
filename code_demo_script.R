
### Load libraries and gapminder data set
library(gapminder)
library(ggplot2)
library(ggthemes)
library(gganimate)
library(tidyverse)
library(plotly)
options(scipen=999)

# load data from gapminder
data <- gapminder


# 1. Data exploration, using base R
#   a. Quick distribution plots
#   b. scatter plots
#   c. adding lines / more points
#   d. Correlation heatmaps


plot(data$lifeExp, data$gdpPercap)
points(data$lifeExp,data$pop)
abline(v=50,h=50000)

cromat <- cor(data[,c(4:6)])

heatmap(cromat)

library(heatmaply)

heatmaply(cromat)

library(pheatmap)

pheatmap(cromat)


p <- ggplot(data=data, aes(x=(gdpPercap), y=(lifeExp), col=continent, size=pop)) +
  scale_x_log10() +
  scale_y_log10() +
  geom_point(alpha=0.5) + 
  # geom_smooth(method = "lm") +
  theme_few() +
  ggtitle("Gapminder plot") +
  xlab("GDP ") +
  ylab("Life expectancy") +
  theme(plot.title = element_text(hjust=0.5))


p + transition_time(year) + labs(title = "Year: {frame_time}")

library(plotly)

ggplotly(p)






  # 2. Create a plot that tells the story of the data, using ggplot
#   a. data
#   b. aesthetics 
#   c. scales
#   d. geometric objects
#   e. statistics
#   f. facets
#   g. coordinates

# 3. Animation with transition_time() from gganimate

# 4. Combining ggplots and tidyverse

# 4. Saving plots



# Base R exploratory plotting ---------------------------------------------

### Distributions

# Histogram
hist(data$gdpPercap)



# Boxplot
boxplot(data$lifeExp)



### Quick joint distribution plot of numeric variables
pairs(data[,4:ncol(data)]) 




### Relationships


# Scatter plot
plot(data$lifeExp,data$gdpPercap)


# add lines to the plot
abline(h=40000,v=60)



# adding more points to a plot
points(data$lifeExp,data$pop)



# adding a linear model - first create the model
mod <- lm(gdpPercap ~ lifeExp, data=data)



# then add to the plot
abline(mod)



### Aligning multiple plots

plot.new()
par(mfrow=c(1,2))
plot(data$lifeExp,data$pop)
boxplot(data$gdpPercap)

# to finish a plot
dev.off()


### To save a plot

png("my_saved_plot")
plot(data$lifeExp,data$gdpPercap)
dev.off()



# Heatmaps ----------------------------------------------------------------



# Create correlation matrix
cormat <- data %>% select_if(is.numeric) %>% cor()

heatmap(cormat)
heatmap(data[,c(4:6)])

library(heatmaply)
heatmaply(cormat)




### GGplot for data visualisation -----------------------------------------


# Data and aesthetics -----------------------------------------------------


### Data and aesthetics
ggplot(data, aes(x=gdpPercap,y=lifeExp))


# Geometric objects -------------------------------------------------------

### Now add geometric objects
ggplot(data, aes(x=gdpPercap,y=lifeExp)) + 
  geom_point()


# Scales ------------------------------------------------------------------

### Change the axis scales to logarithmic
ggplot(data, aes(x=log10(gdpPercap),y =log10(lifeExp))) + 
  geom_point()

  # but - the axes labels are now non-informative. This is a better way:
  
  ggplot(data, aes(x=gdpPercap,y=lifeExp)) + 
    geom_point()+
    scale_x_log10() +
    scale_y_log10()


  

# Facets ------------------------------------------------------------------


### Now separate out the continents
ggplot(data, aes(x = gdpPercap,y =lifeExp,
                 col = continent)) +
  geom_point() +
  scale_x_log10() +
  scale_y_log10()

  
  ### Or like this
  ggplot(data, aes(x = gdpPercap,y =lifeExp,
                   col = continent)) +
    geom_point() +
    scale_x_log10() +
    scale_y_log10() +
    facet_wrap(.~continent)
  

# Aesthetics (revisited) --------------------------------------------------

  
### A bit cluttered - change the transparency
ggplot(data, aes(x = gdpPercap,y =lifeExp, col = continent)) +
  geom_point(alpha=0.5) +
  scale_x_log10() +
  scale_y_log10()


### Now add in the element of population
ggplot(data, aes(x = gdpPercap,y =lifeExp, size=pop, col = continent)) +
  geom_point(alpha=0.5) +
  scale_x_log10() +
  scale_y_log10()


### Try a different theme
ggplot(data, aes(x = gdpPercap,y =lifeExp, size=pop, col = continent)) +
  geom_point(alpha=0.5) +
  scale_x_log10() +
  scale_y_log10() +
  theme_few()


# Coordinates -------------------------------------------------------------

### Try new coordinates 

ggplot(data, aes(x = gdpPercap,y =lifeExp, size=pop, col = continent)) +
  geom_point(alpha=0.5) +
  scale_x_log10() +
  scale_y_log10() +
  theme_few() + 
  coord_cartesian(xlim=c(100,500000), ylim=c(20,85)) 




# Animation: showing change over time -------------------------------------

### save the static plot as an object
p <- ggplot(data, aes(x = gdpPercap,y =lifeExp, size=pop, col = continent)) +
      geom_point(alpha=0.5) +
      scale_x_log10() +
      scale_y_log10() +
      theme_few() + 
  scale_size(range = c(2, 20)) +
  coord_cartesian(xlim=c(100,500000), ylim=c(20,85))


### then apply a transition time (from gganimate) to the ggplot object
p + transition_time(year)



### add an annotation for the year
p + transition_time(year) + labs(title = "Year: {frame_time}")




# Plotly wrapping ---------------------------------------------------------

library(plotly)

### To create an interactive HTML plot:

### create the static plot 
p <- ggplot(data, aes(x = gdpPercap,y =lifeExp, size=pop, col = continent)) +
  geom_point(show.legend = FALSE, alpha=0.5) +
  scale_x_log10() +
  scale_y_log10() +
  theme_few() + 
  scale_size(range = c(2, 5)) +
  coord_cartesian(xlim=c(100,500000), ylim=c(20,85))

### then wrap in ggplotly
ggplotly(p)




# Combining ggplot and tidyverse ------------------------------------------

### Load data from comp epi practical
proteins <- readRDS("/Users/mathzero/Google\ Drive/Imperial/HDA_2020/TEACHING/ggplot/04-practical/
                    BCL_analyses/Data/Proteins.rds")
covars <- readRDS("/Users/mathzero/Google\ Drive/Imperial/HDA_2020/TEACHING/ggplot/04-practical/
                  BCL_analyses/Data/Covariates.rds")
ce.data <- merge(covars,proteins, by="row.names")


#####  melt data set for facet wrapping ##

### gather = from wide to long
### spread = from long to wide

library(tidyverse)
ce.data.long <- ce.data[,c(names(proteins), "type")] %>% gather(key,value,-type)


### Plot facet plot
ggplot(ce.data.long) + 
  geom_density(aes(x=value,col=type, fill=type), alpha=0.5) + 
  facet_wrap(.~key) +
  theme_tufte()







# Saving ggplots ----------------------------------------------------------

### You can use the same method as regular plots

### Or - ggsave. Can take a last_plot() command to save the last plot you generated

ggsave("my_ggsaved_plot.png", plot = last_plot(), width = 8, height = 7, 
       units = "in",dpi = 300)





# Plotting missing data ---------------------------------------------------

### With ggplot

# create some dummy data
df = data.frame(matrix(rnorm(200), nrow=10))
# add some random NAs
for (i in 1:40){
  set.seed(i)
  df[sample(nrow(df), 1),sample(ncol(df), 1)] <- NA
}

# create a simple summary data frame of missing values
df.missing <- as.data.frame(colSums(is.na(df)))
names(df.missing) <- "na_count"
df.missing$variable <- rownames(df.missing)


### simple missing plot of missing data by variable
ggplot(data=df.missing) + 
  geom_col(aes(x=reorder(variable,na_count), y=na_count)) +
  coord_flip()




### Or - the easy way
library(DataExplorer)
plot_missing(df)




### Or an upset plot to show intersections of missing data
library(naniar)
gg_miss_upset(df, nsets = 10)



# A bit of fun: XKCD ------------------------------------------------------

### there is a package that allows you to create plots in XKCD stlye

library(xkcd)
library(extrafont)
library(ggplot2)


xrange <- range(results$coef)
yrange <- range(-log10(results$pval))
set.seed(123) # for reproducibility
ggplot(results) + geom_point(aes(coef, -log10(results$pval)), data=results) + xkcdaxis(xrange,yrange) + theme(text = element_text(size =16, family = "xkcd")) + ylab("-log10 p-value") + xlab("Beta coefficient")





# A useful code chunk -----------------------------------------------------


### this chunk of code plots the distribution of all the numeric variables in your data set


results %>%
  keep(is.numeric) %>%                     # Keep only numeric columns
  gather() %>%                             # Convert to key-value pairs
  ggplot(aes(value)) +                     # Plot the values
  facet_wrap(~ key, scales = "free") +   # In separate panels
  geom_density() + 
  theme_clean()

# ggsave(filename = "stats_plots_pre_clean.png",plot = last_plot(),width = 16, 
#        height = 12, dpi = 300, units = "in", device='png')


