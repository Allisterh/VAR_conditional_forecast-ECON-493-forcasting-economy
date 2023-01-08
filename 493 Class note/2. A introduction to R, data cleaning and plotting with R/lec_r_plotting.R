## ----setup, include=FALSE------------------------------------------------------
knitr::opts_chunk$set(
  echo=TRUE, cache=FALSE, warning=FALSE, message=FALSE, 
  fig.align="center",
  fig.width=7,
  fig.height=3.5,
  out.width='99%'
)

# packages
library(fontawesome)
library(pacman)
p_load(tidyverse, gapminder)

# 
options(digits = 4)

# set transparent background 
theme_set(theme_grey())
theme_update(
  plot.background = element_rect(fill = "transparent", colour = NA),
  legend.background = element_rect(fill = "transparent", colour = NA)
)

# grey, blue, orange, green, yellow
my_colors <- c("#606060", "#3C78B0", "#D55E00", "#64B4C2", "#E69F00")

# set seed
set.seed(1234)



## ---- eval=FALSE---------------------------------------------------------------
## # load library
## library(gapminder)


## ------------------------------------------------------------------------------
# check out data
str(gapminder)


## ---- message=FALSE------------------------------------------------------------
library(dplyr)
China <- gapminder %>% filter(country == "China") 
head(China, 4)


## ------------------------------------------------------------------------------
plot(lifeExp ~ year, data = China, 
     xlab = "Year", ylab = "Life expectancy",
     main = "Life expectancy in China", 
     col = "red", cex.lab = 1, cex.main= 1, pch = 16)


## ------------------------------------------------------------------------------
library(ggplot2)


## ------------------------------------------------------------------------------
#
ggplot(data = China, aes(x = year, y = lifeExp)) +
  geom_point()


## ------------------------------------------------------------------------------
lifeExp_China <- 
  ggplot(data = China, aes(x = year, y = lifeExp)) +
  geom_point()


## ------------------------------------------------------------------------------
#
lifeExp_China


## ------------------------------------------------------------------------------
# 
lifeExp_China + theme_bw()


## ------------------------------------------------------------------------------
# 
lifeExp_China <- ggplot(data = China, aes(x = year, y = lifeExp)) 
# 
# 
# 
# 
# 
# 


## ------------------------------------------------------------------------------
# 
lifeExp_China <- ggplot(data = China, aes(x = year, y = lifeExp)) + 
  geom_point()
# 
# 
# 
# 
# 


## ------------------------------------------------------------------------------
# 
lifeExp_China <- ggplot(data = China, aes(x = year, y = lifeExp)) + 
  geom_point(color = "red", size = 2)
# 
# 
# 
# 
# 


## ------------------------------------------------------------------------------
# 
lifeExp_China <- ggplot(data = China, aes(x = year, y = lifeExp)) + 
  geom_point(color = "red", size = 2) +
  xlab("Year") 
# 
# 
# 
# 


## ------------------------------------------------------------------------------
# 
lifeExp_China <- ggplot(data = China, aes(x = year, y = lifeExp)) + 
  geom_point(color = "red", size = 2) +
  xlab("Year") + 
  ylab("Life expectancy")
# 
# 
# 


## ------------------------------------------------------------------------------
# 
lifeExp_China <- ggplot(data = China, aes(x = year, y = lifeExp)) + 
  geom_point(color = "red", size = 2) +
  xlab("Year") + 
  ylab("Life expectancy") +
  ggtitle("Life expectancy in China")
# 
# 


## ------------------------------------------------------------------------------
# 
lifeExp_China <- ggplot(data = China, aes(x = year, y = lifeExp)) + 
  geom_point(color = "red", size = 2) +
  xlab("Year") + 
  ylab("Life expectancy") +
  ggtitle("Life expectancy in China") +
  theme_bw()
# 


## ------------------------------------------------------------------------------
# 
lifeExp_China


## ------------------------------------------------------------------------------
# 
lifeExp_by_year <- 
  ggplot(
    data = gapminder, 
    aes(x = year, y = lifeExp)
  ) +
  geom_point(color = "red", size = 2) +
  xlab("Year") + 
  ylab("Life expectancy") +
  ggtitle("Life expectancy over time") 


## ------------------------------------------------------------------------------
# 
lifeExp_by_year


## ------------------------------------------------------------------------------
# 
lifeExp_by_year <- 
  ggplot(
    data = gapminder, 
    aes(x = year, y = lifeExp)
  ) +
  geom_line(color = "red", size = 2) +
  xlab("Year") + 
  ylab("Life expectancy") +
  ggtitle("Life expectancy over time") 


## ------------------------------------------------------------------------------
# 
lifeExp_by_year


## ------------------------------------------------------------------------------
# 
lifeExp_by_year <- 
  ggplot(
    data = gapminder, 
    aes(x = year, y = lifeExp, group = country)
  ) +
  geom_line(color = "red", size = 2) +
  xlab("Year") + 
  ylab("Life expectancy") +
  ggtitle("Life expectancy over time") 


## ------------------------------------------------------------------------------
# 
lifeExp_by_year


## ------------------------------------------------------------------------------
# 
lifeExp_by_year <- 
  ggplot(
    data = gapminder,
    aes(x = year, y = lifeExp, group = country)
  ) +
  geom_line(color = "red") +
  xlab("Year") + 
  ylab("Life expectancy") +
  ggtitle("Life expectancy over time") 


## ------------------------------------------------------------------------------
# 
lifeExp_by_year


## ------------------------------------------------------------------------------
# 
lifeExp_by_year <- 
  ggplot(
    data = gapminder,
    aes(x = year, y = lifeExp, group = country, color = continent)
  ) +
  geom_line() +
  xlab("Year") + 
  ylab("Life expectancy") +
  ggtitle("Life expectancy over time") 


## ------------------------------------------------------------------------------
# 
lifeExp_by_year


## ------------------------------------------------------------------------------
# 
lifeExp_by_year <- 
  ggplot(
    data = gapminder,
    aes(x = year, y = lifeExp, group = country, color = continent)
  ) +
  geom_line() +
  xlab("Year") + 
  ylab("Life expectancy") +
  ggtitle("Life expectancy over time") +
  facet_wrap(~ continent)


## ------------------------------------------------------------------------------
# 
lifeExp_by_year


## ------------------------------------------------------------------------------
# 
lifeExp_by_year <- 
  ggplot(
    data = gapminder,
    aes(x = year, y = lifeExp, group = country, color = continent)
  ) +
  geom_line() +
  xlab("Year") + 
  ylab("Life expectancy") +
  ggtitle("Life expectancy over time") +
  facet_wrap(~ continent) +
  theme(legend.position = "none")


## ------------------------------------------------------------------------------
# 
lifeExp_by_year


## ------------------------------------------------------------------------------
ggplot(data = China, aes(x = year, y = lifeExp)) +
  geom_line() +
  ggtitle("Chinese life expectancy") +
  theme_gray(base_size = 20)


## ------------------------------------------------------------------------------
lifeExp_by_year + theme(legend.position = c(0.8, 0.2)) +
  scale_color_manual(
    name = "Which continent are\nwe looking at?", # \n adds a line break
    values = c("Africa" = "seagreen", "Americas" = "turquoise1",
               "Asia" = "royalblue", "Europe" = "violetred1", "Oceania" = "yellow"))


## ------------------------------------------------------------------------------
# 
lifeExp_by_year + theme_minimal() + theme(legend.position = "none")


## ------------------------------------------------------------------------------
# 
lifeExp_by_year + theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45),
    legend.position = "none"
  )


## ---- eval=FALSE---------------------------------------------------------------
## ggsave(
##   "I_saved_a_file.pdf",
##   plot = lifeExp_by_year,
##   height = 3, width = 5, units = "in"
## )


## ------------------------------------------------------------------------------
# gapminder scatterplot
ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(aes(size = pop, col = continent), alpha = 0.3)


## ----layers--------------------------------------------------------------------
# logarithmic scale on x-axis, use dollar units
p <- ggplot(data = gapminder, aes(x = gdpPercap, y = lifeExp)) +
  geom_point(aes(size = pop, col = continent), alpha = 0.3) +
  scale_color_brewer(name = "Continent", palette = "Set1") + 
  scale_size(name = "Population", labels = scales::comma) + 
  scale_x_log10(labels = scales::dollar) + 
  labs(x = "Log (GDP per capita)", y = "Life Expectancy") + 
  theme_minimal() 


## ---- echo=F-------------------------------------------------------------------
# a nicer plot 
p 


## ---- eval=FALSE---------------------------------------------------------------
## ggplot(data, aes(x = time, y = ATT)) +
##   geom_hline(yintercept = 0, color = "grey50", size = .5) +
##   geom_vline(xintercept = 0, color = "grey25", linetype = "dashed", size = .75) +
##   geom_ribbon(aes(x = time, ymin = CI.lower, ymax = CI.upper), fill = "grey25", alpha = .2) +
##   geom_line(size = .5, color = my_colors[3]) +
##   geom_point(size = .5, color = my_colors[3]) +
##   scale_y_continuous(name = "") +
##   scale_x_continuous(name = "", limits = c(-12,4)) +
##   theme(legend.position = "none") +
##   facet_wrap(
##     ~ factor(
##       crime,
##       levels = c("Assault", "Burglary", "Robbery","Theft", "AutoTheft", "Homicide")
##     )
##   )

