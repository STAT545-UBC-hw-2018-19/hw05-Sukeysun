Homework 05: Factor and figure management
================
Sukey
October 11, 2018

-   [Part 1: Factor management](#part-1-factor-management)
    -   [Elaboration for the gapminder data set](#elaboration-for-the-gapminder-data-set)
        -   [Drop Oceania.](#drop-oceania.)
        -   [Reorder the levels of country or continent](#reorder-the-levels-of-country-or-continent)
-   [Part 2: File I/O](#part-2-file-io)
    -   [write\_csv()/read\_csv()](#write_csvread_csv)
    -   [write.csv()/read.csv()](#write.csvread.csv)
    -   [saveRDS()/readRDS()](#saverdsreadrds)
    -   [dput()/dget()](#dputdget)
-   [Part 3: Visualization design](#part-3-visualization-design)
    -   [orignial plot in my past assignment](#orignial-plot-in-my-past-assignment)
    -   [Modified graph](#modified-graph)
    -   [plotly](#plotly)
-   [Part 4: Writing figures to file](#part-4-writing-figures-to-file)
    -   [write figure](#write-figure)
    -   [load figure](#load-figure)
-   [But I want to do more!](#but-i-want-to-do-more)

Part 1: Factor management
=========================

Elaboration for the gapminder data set
--------------------------------------

**Follwings are my preparation work**

``` r
## load all packages
suppressPackageStartupMessages(library(tidyverse))
suppressPackageStartupMessages(library(plotly))  
suppressPackageStartupMessages(library(gapminder))
suppressPackageStartupMessages(library(knitr))
suppressPackageStartupMessages(library(forcats))
suppressPackageStartupMessages(library(gridExtra))
suppressPackageStartupMessages(library(grid))
suppressPackageStartupMessages(library(gtable))
suppressPackageStartupMessages(library(scales))

## set the table theme 

myt <- ttheme_default( 
         # Use hjust and x to center the text
         # Alternate the row fill colours
                 core = list( fg_params=list( hjust = 0.5 , x = 0.5 ),   # 1: right 0:left 0.5:center
                             bg_params=list( fill=c( "green", "pink" ) ) ),

         # Change column header to white text and red background
                 colhead = list( fg_params=list( col="white" ),
                                bg_params=list( fill="red" ) ) 
         )
```

### Drop Oceania.

Filter the Gapminder data to remove observations associated with the continent of Oceania. Additionally, remove unused factor levels. Provide concrete information on the data before and after removing these rows and Oceania; address the number of rows and the levels of the affected factors.

**First, let's check the original structure of gapminder**

``` r
## the original structure of gapminder:

### convert the ouput of str() to table
data.frame( variable = names( gapminder ),
           classes = sapply( gapminder, class ),
           factorlevel = sapply( gapminder, nlevels ),
           first_values = sapply( gapminder, function( x ) paste0( head( x ),  collapse = ", ") ),
           row.names = NULL ) %>% 
  kable()
```

| variable  | classes |  factorlevel| first\_values                                                                |
|:----------|:--------|------------:|:-----------------------------------------------------------------------------|
| country   | factor  |          142| Afghanistan, Afghanistan, Afghanistan, Afghanistan, Afghanistan, Afghanistan |
| continent | factor  |            5| Asia, Asia, Asia, Asia, Asia, Asia                                           |
| year      | integer |            0| 1952, 1957, 1962, 1967, 1972, 1977                                           |
| lifeExp   | numeric |            0| 28.801, 30.332, 31.997, 34.02, 36.088, 38.438                                |
| pop       | integer |            0| 8425333, 9240934, 10267083, 11537966, 13079460, 14880372                     |
| gdpPercap | numeric |            0| 779.4453145, 820.8530296, 853.10071, 836.1971382, 739.9811058, 786.11336     |

As shown in the output above, we can notice that country is a factor with 142 levels and continent is a factor with 5 levels.

**Then,let's check if there is any difference after removing all country in Oceania.**

``` r
## remove all rows whose country is Oceania from gapminder
gapRemove <- gapminder %>% 
  filter( continent != "Oceania" ) %>% 
  droplevels() 
```

    ## Warning: package 'bindrcpp' was built under R version 3.5.1

``` r
## show the tables before and after removing Oceania
grid.arrange(top = " Factor count comparison of gapminder and gapRemove",
             tableGrob(fct_count(gapRemove$continent),
                       theme = myt,
                       rows = NULL),
             tableGrob(fct_count(gapminder$continent),
                       theme = myt,
                       rows = NULL),
                       nrow = 1)
```

![](Factor_and_figure_management_files/figure-markdown_github/remove%20oceania-1.png)

``` r
## check the number of rows of each dataframe                         
nrow(gapminder)
```

    ## [1] 1704

``` r
nrow(gapRemove)
```

    ## [1] 1680

The left table is the factor count of gapRemove, compared with the right one, we can notice that continent "Oceania" has been removed. The total row of gapminder is 1704 while the total row of gapRemove is 1680, and the difference between the total row number of these two dataframe is 24, which is just the "Oceania" count number in the right table.

### Reorder the levels of country or continent

Use the forcats package to change the order of the factor levels, based on a principled summary of one of the quantitative variables. Consider experimenting with a summary statistic beyond the most basic choice of the median.

``` r
lifeExp.mean.fig <- gapminder %>%
  ## reorder the continent by the mean value of life expectancy
  mutate( continent = fct_reorder( continent, lifeExp, mean ) ) %>% 
  ## plot gapminder
  ggplot( aes( continent, lifeExp ) )+
  geom_boxplot( aes( fill = continent ) ) +
  ## statistic summary, show the mean value
  stat_summary( fun.y=mean, colour="darkred", geom="point", size=2,show.legend  = TRUE ) +
  stat_summary( fun.y=mean, colour="white", geom="text", size = 3, show.legend  = TRUE, 
               vjust=-0.7, aes( label=round( ..y.., digits=1 ) ) ) +
  ggtitle(" Reorder by the mean value of life expectancy ") +
  ## center ggtitle
  theme(plot.title = element_text(hjust = 0.5))

## calculate the mean lifeExp of each continent and sort by the mean value
lifeExp.mean.table <- gapminder %>% 
  group_by( continent ) %>%
  summarize( meanlifeExp = mean( lifeExp ) ) %>% 
  arrange( meanlifeExp ) 

grid.arrange( tableGrob( lifeExp.mean.table,
                         theme = myt,
                         rows = NULL),
             lifeExp.mean.fig,
             nrow = 1)
```

![](Factor_and_figure_management_files/figure-markdown_github/Reorder%20by%20the%20mean%20value%20of%20life%20expectancy-1.png)

As shown in the table and graph above, gapminder is reorder according to the mean of lifeExp of each continent. We can notice that the mean life expectancy of Oceania and Europe are pretty similar and higher than mean life expectancy of other continents while Africa has the lowest life expectancy mean which is only 48.9 years old.

``` r
gdp.mean.fig <- gapminder %>%
  ## reorder the continent by the mean value of gdpPercap
  mutate( continent = fct_reorder( continent, gdpPercap, mean ) ) %>% 
  ## plot gapminder
  ggplot( aes( continent, gdpPercap ) )+
  geom_boxplot( aes( fill = continent ) ) +
  ## statistic summary, show the mean value
  stat_summary( fun.y=mean, colour="darkred", geom="point", size=2,show.legend  = TRUE ) +
  stat_summary( fun.y=mean, colour="red", geom="text", size = 4, show.legend  = TRUE, 
               vjust=-0.7, aes( label=round( ..y.., digits=1 ) ) ) +
  ggtitle(" Reorder by the mean value of gdpPercap ") +
  ## center ggtitle
  theme(plot.title = element_text(hjust = 0.5))

## calculate the mean gdpPercap of each continent and sort by the mean value
gdp.mean.table <- gapminder %>% 
  group_by( continent ) %>%
  summarize( meangdpPercap = mean( gdpPercap ) ) %>% 
  arrange( meangdpPercap ) 

grid.arrange( tableGrob( gdp.mean.table,
                         theme = myt,
                         rows = NULL),
             gdp.mean.fig,
             nrow = 1)
```

![](Factor_and_figure_management_files/figure-markdown_github/Reorder%20by%20the%20mean%20value%20of%20gdp-capita-1.png) As shown in the table and graph above, gapminder is reorder according to the mean of gdp/capita of each continent. We can notice that the gdp/capita mean of Oceania is much higher than the gdp/capita mean of other continents and Africa has the lowest gdp/capita mean which is only around 2193.

Part 2: File I/O
================

Experiment with one or more of write\_csv()/read\_csv() (and/or TSV friends), saveRDS()/readRDS(), dput()/dget(). Create something new, probably by filtering or grouped-summarization of Singer or Gapminder. I highly recommend you fiddle with the factor levels, i.e. make them non-alphabetical (see previous section). Explore whether this survives the round trip of writing to file then reading back in.

write\_csv()/read\_csv()
------------------------

**I am interested data in Oceania since although it has the smallest number of countries, mean life expectancy and gdp/capita of it is the highest. I would like to save the data of Oceania locally but I also want to reorder the data in terms of year instead of the initial of each country**

``` r
Oceania <- gapminder %>%
  filter( continent == "Oceania") %>% 
  arrange( year ) %>% 
  droplevels()
  
grid.arrange( top = " Top 10 rows of data in Oceania",
              tableGrob( head( Oceania, 10 ),
                         theme = myt,
                         rows = NULL))
```

![](Factor_and_figure_management_files/figure-markdown_github/extract%20all%20data%20in%20Oceania-1.png) **Save all data in Oceania with a new order locally**

``` r
write_csv(Oceania, "Oceania.csv")
```

**load Oceania.csv file**

``` r
Oceania.read <- read_csv("Oceania.csv")
```

    ## Parsed with column specification:
    ## cols(
    ##   country = col_character(),
    ##   continent = col_character(),
    ##   year = col_integer(),
    ##   lifeExp = col_double(),
    ##   pop = col_integer(),
    ##   gdpPercap = col_double()
    ## )

**let's check if there is any difference between Oceania and Oceania.read**

``` r
## check the structure of each dataframe
data.frame( variable = names( Oceania ),
           classes = sapply( Oceania, class ),
           factorlevel = sapply( Oceania, nlevels ),
           first_values = sapply( Oceania, function( x ) paste0( head( x ),  collapse = ", ") ),
           row.names = NULL ) %>% 
  kable()
```

| variable  | classes |  factorlevel| first\_values                                                              |
|:----------|:--------|------------:|:---------------------------------------------------------------------------|
| country   | factor  |            2| Australia, New Zealand, Australia, New Zealand, Australia, New Zealand     |
| continent | factor  |            1| Oceania, Oceania, Oceania, Oceania, Oceania, Oceania                       |
| year      | integer |            0| 1952, 1952, 1957, 1957, 1962, 1962                                         |
| lifeExp   | numeric |            0| 69.12, 69.39, 70.33, 70.26, 70.93, 71.24                                   |
| pop       | integer |            0| 8691212, 1994794, 9712569, 2229407, 10794968, 2488550                      |
| gdpPercap | numeric |            0| 10039.59564, 10556.57566, 10949.64959, 12247.39532, 12217.22686, 13175.678 |

``` r
data.frame( variable = names( Oceania.read ),
           classes = sapply( Oceania.read, class ),
           factorlevel = sapply( Oceania.read, nlevels ),
           first_values = sapply( Oceania.read, function( x ) paste0( head( x ),  collapse = ", ") ),
           row.names = NULL ) %>% 
  kable()
```

| variable  | classes   |  factorlevel| first\_values                                                              |
|:----------|:----------|------------:|:---------------------------------------------------------------------------|
| country   | character |            0| Australia, New Zealand, Australia, New Zealand, Australia, New Zealand     |
| continent | character |            0| Oceania, Oceania, Oceania, Oceania, Oceania, Oceania                       |
| year      | integer   |            0| 1952, 1952, 1957, 1957, 1962, 1962                                         |
| lifeExp   | numeric   |            0| 69.12, 69.39, 70.33, 70.26, 70.93, 71.24                                   |
| pop       | integer   |            0| 8691212, 1994794, 9712569, 2229407, 10794968, 2488550                      |
| gdpPercap | numeric   |            0| 10039.59564, 10556.57566, 10949.64959, 12247.39532, 12217.22686, 13175.678 |

As shown in the two tables above, the conutry and continent have been converted to character.**A better way is to use wirte.csv() and read.csv()**

write.csv()/read.csv()
----------------------

``` r
write.csv( Oceania, file = "Oceanianew.csv" , row.names = FALSE)
Oceanianew.csv.read <- read.csv( "Oceanianew.csv" )

## check the structure of each dataframe
data.frame( variable = names( Oceania ),
           classes = sapply( Oceania, class ),
           factorlevel = sapply( Oceania, nlevels ),
           first_values = sapply( Oceania, function( x ) paste0( head( x ),  collapse = ", ") ),
           row.names = NULL ) %>% 
  kable()
```

| variable  | classes |  factorlevel| first\_values                                                              |
|:----------|:--------|------------:|:---------------------------------------------------------------------------|
| country   | factor  |            2| Australia, New Zealand, Australia, New Zealand, Australia, New Zealand     |
| continent | factor  |            1| Oceania, Oceania, Oceania, Oceania, Oceania, Oceania                       |
| year      | integer |            0| 1952, 1952, 1957, 1957, 1962, 1962                                         |
| lifeExp   | numeric |            0| 69.12, 69.39, 70.33, 70.26, 70.93, 71.24                                   |
| pop       | integer |            0| 8691212, 1994794, 9712569, 2229407, 10794968, 2488550                      |
| gdpPercap | numeric |            0| 10039.59564, 10556.57566, 10949.64959, 12247.39532, 12217.22686, 13175.678 |

``` r
data.frame( variable = names( Oceanianew.csv.read ),
           classes = sapply( Oceanianew.csv.read, class ),
           factorlevel = sapply( Oceanianew.csv.read, nlevels ),
           first_values = sapply( Oceanianew.csv.read, function( x ) paste0( head( x ),  collapse = ", ") ),
           row.names = NULL ) %>% 
  kable()
```

| variable     | classes    |     factorlevel| first\_values                                                              |
|:-------------|:-----------|---------------:|:---------------------------------------------------------------------------|
| country      | factor     |               2| Australia, New Zealand, Australia, New Zealand, Australia, New Zealand     |
| continent    | factor     |               1| Oceania, Oceania, Oceania, Oceania, Oceania, Oceania                       |
| year         | integer    |               0| 1952, 1952, 1957, 1957, 1962, 1962                                         |
| lifeExp      | numeric    |               0| 69.12, 69.39, 70.33, 70.26, 70.93, 71.24                                   |
| pop          | integer    |               0| 8691212, 1994794, 9712569, 2229407, 10794968, 2488550                      |
| gdpPercap    | numeric    |               0| 10039.59564, 10556.57566, 10949.64959, 12247.39532, 12217.22686, 13175.678 |
| Now the stru | cture of O |  ceania and Oce| anianew are same.                                                          |

saveRDS()/readRDS()
-------------------

``` r
saveRDS( Oceania,"Oceania.rds" )
Oceania.rds.read <- readRDS( "Oceania.rds" )

## check the structure of each dataframe
data.frame( variable = names( Oceania ),
           classes = sapply( Oceania, class ),
           factorlevel = sapply( Oceania, nlevels ),
           first_values = sapply( Oceania, function( x ) paste0( head( x ),  collapse = ", ") ),
           row.names = NULL ) %>% 
  kable()
```

| variable  | classes |  factorlevel| first\_values                                                              |
|:----------|:--------|------------:|:---------------------------------------------------------------------------|
| country   | factor  |            2| Australia, New Zealand, Australia, New Zealand, Australia, New Zealand     |
| continent | factor  |            1| Oceania, Oceania, Oceania, Oceania, Oceania, Oceania                       |
| year      | integer |            0| 1952, 1952, 1957, 1957, 1962, 1962                                         |
| lifeExp   | numeric |            0| 69.12, 69.39, 70.33, 70.26, 70.93, 71.24                                   |
| pop       | integer |            0| 8691212, 1994794, 9712569, 2229407, 10794968, 2488550                      |
| gdpPercap | numeric |            0| 10039.59564, 10556.57566, 10949.64959, 12247.39532, 12217.22686, 13175.678 |

``` r
data.frame( variable = names( Oceania.rds.read ),
           classes = sapply( Oceania.rds.read, class ),
           factorlevel = sapply( Oceania.rds.read, nlevels ),
           first_values = sapply( Oceania.rds.read, function( x ) paste0( head( x ),  collapse = ", ") ),
           row.names = NULL ) %>% 
  kable()
```

| variable     | classes    |   factorlevel| first\_values                                                              |
|:-------------|:-----------|-------------:|:---------------------------------------------------------------------------|
| country      | factor     |             2| Australia, New Zealand, Australia, New Zealand, Australia, New Zealand     |
| continent    | factor     |             1| Oceania, Oceania, Oceania, Oceania, Oceania, Oceania                       |
| year         | integer    |             0| 1952, 1952, 1957, 1957, 1962, 1962                                         |
| lifeExp      | numeric    |             0| 69.12, 69.39, 70.33, 70.26, 70.93, 71.24                                   |
| pop          | integer    |             0| 8691212, 1994794, 9712569, 2229407, 10794968, 2488550                      |
| gdpPercap    | numeric    |             0| 10039.59564, 10556.57566, 10949.64959, 12247.39532, 12217.22686, 13175.678 |
| Nothing chan | ge because |  the class of| data is preserved.                                                         |

dput()/dget()
-------------

``` r
dput( Oceania,"Oceania.txt" )
Oceania.txt.read <- dget( "Oceania.txt" )

## check the structure of each dataframe
data.frame( variable = names( Oceania ),
           classes = sapply( Oceania, class ),
           factorlevel = sapply( Oceania, nlevels ),
           first_values = sapply( Oceania, function( x ) paste0( head( x ),  collapse = ", ") ),
           row.names = NULL ) %>% 
  kable()
```

| variable  | classes |  factorlevel| first\_values                                                              |
|:----------|:--------|------------:|:---------------------------------------------------------------------------|
| country   | factor  |            2| Australia, New Zealand, Australia, New Zealand, Australia, New Zealand     |
| continent | factor  |            1| Oceania, Oceania, Oceania, Oceania, Oceania, Oceania                       |
| year      | integer |            0| 1952, 1952, 1957, 1957, 1962, 1962                                         |
| lifeExp   | numeric |            0| 69.12, 69.39, 70.33, 70.26, 70.93, 71.24                                   |
| pop       | integer |            0| 8691212, 1994794, 9712569, 2229407, 10794968, 2488550                      |
| gdpPercap | numeric |            0| 10039.59564, 10556.57566, 10949.64959, 12247.39532, 12217.22686, 13175.678 |

``` r
data.frame( variable = names( Oceania.txt.read ),
           classes = sapply( Oceania.txt.read, class ),
           factorlevel = sapply( Oceania.txt.read, nlevels ),
           first_values = sapply( Oceania.txt.read, function( x ) paste0( head( x ),  collapse = ", ") ),
           row.names = NULL ) %>% 
  kable()
```

| variable  | classes |  factorlevel| first\_values                                                              |
|:----------|:--------|------------:|:---------------------------------------------------------------------------|
| country   | factor  |            2| Australia, New Zealand, Australia, New Zealand, Australia, New Zealand     |
| continent | factor  |            1| Oceania, Oceania, Oceania, Oceania, Oceania, Oceania                       |
| year      | integer |            0| 1952, 1952, 1957, 1957, 1962, 1962                                         |
| lifeExp   | numeric |            0| 69.12, 69.39, 70.33, 70.26, 70.93, 71.24                                   |
| pop       | integer |            0| 8691212, 1994794, 9712569, 2229407, 10794968, 2488550                      |
| gdpPercap | numeric |            0| 10039.59564, 10556.57566, 10949.64959, 12247.39532, 12217.22686, 13175.678 |

Part 3: Visualization design
============================

orignial plot in my past assignment
-----------------------------------

This is the task 2 in assignment 3 : Look at the spread of GDP per capita within the continents.

``` r
range_captia <- gapminder %>%
  group_by( continent ) %>% 
  summarize( min_gdp = min( gdpPercap ), max_gdp = max( gdpPercap ),
            mean_gdp = mean( gdpPercap ), md_gdp = median( gdpPercap ),
            sd_gdp = sd( gdpPercap )) %>%  # summarize the basic info of gdp in each continent
  mutate( gdp_range = paste( min_gdp, max_gdp, sep = " ~ ")) %>% # separate minimum and maximum value by ~
  select( continent, gdp_range, mean_gdp, md_gdp, sd_gdp)


spreadplt <- ggplot( gapminder, aes( continent, gdpPercap )) +
  geom_boxplot( aes( fill = continent ))+
  labs( x = "continent", y = "gdp/capita", 
              title = "spread of GDP per capita within the continents") +
  theme(plot.title = element_text(size=22))

rangeplt <- range_captia %>%
  ggplot() +
  geom_line( aes( x = continent, y = mean_gdp, color = "mean",group = 1 ))+
  geom_line( aes( x = continent,y = md_gdp,color = "median",group = 1 )) +
  geom_line( aes( x = continent,y = sd_gdp,color = "standard deviation",group = 1 ))+
  labs(x = "continent", y = "gdp/capita", 
              title = "spread of GDP per capita within the continents") +
  theme(plot.title = element_text(size=22))
  
grid.arrange(tableGrob(range_captia),
             spreadplt,
             rangeplt,
             nrow = 2)
```

![](Factor_and_figure_management_files/figure-markdown_github/past%20plot-1.png)

Modified graph
--------------

I would like to improve the graph above.

``` r
spreadplt.new <- gapminder %>%
  mutate( continent = fct_reorder( continent, gdpPercap, mean ) ) %>% 
  ggplot( aes( x = continent, y = gdpPercap ) )+
  geom_boxplot( aes( colour = continent ),
                outlier.alpha = 0.1 )+
  ## add mean value on the graph
  stat_summary( fun.y=mean, colour="darkred", geom="point", size=2,show.legend  = TRUE ) +
  stat_summary( fun.y=mean, colour="red", geom="text", size = 4, show.legend  = TRUE, 
               vjust=-0.7, aes( label=round( ..y.., digits=1 ) ) ) +
  ## add "$"
  scale_y_continuous(labels=dollar_format()) +
  theme_bw()+
  
  
  labs( x = "continent", y = "gdp/capita") +
  theme(
         axis.text.x = element_text( size = 12 ),
         axis.text.y = element_text( size = 12 ),
         axis.title = element_text( size = 14),
         legend.text = element_text( size = 14 ),
         legend.title = element_text( size = 15),
         plot.background = element_rect( fill = "white", colour = "grey50" ),
         panel.border = element_rect(linetype = "dashed", fill = NA)
         )


new_colors <- c("mean" = "black", "median" = "blue","standard deviation" = "red" )
rangeplt.new <- range_captia %>%
  ggplot() +
  geom_line( aes( x = continent, y = mean_gdp, color = "mean",group = 1 ))+
  geom_line( aes( x = continent,y = md_gdp,color = "median",group = 1 )) +
  
  geom_line( aes( x = continent,y = sd_gdp,color = "standard deviation",group = 1 ))+
  scale_y_sqrt(labels=dollar_format())+
  
  labs(x = "continent", y = "gdp/capita") +
  
 
  
  scale_colour_manual( "Statistic summary:",  
                       values = new_colors,
                       breaks = c("mean", "median","standard deviation" ), # The original values 
                       labels = c(" The Mean", "The Median","The Stdev")   # What you want to call them 
  )+
  theme_bw()+
   theme(
         axis.text.x = element_text( size = 10 ),
         axis.text.y = element_text( size = 10 ),
         axis.title = element_text( size = 14),
         legend.text = element_text( size = 14 ),
         legend.title = element_text( size = 15),
         plot.background = element_rect( fill = "white", colour = "black" ),
         panel.border = element_rect(linetype = "dashed", fill = NA)
         )

  
graph <- grid.arrange( top=textGrob( "Spread of GDP per capita within the continents",
                            gp=gpar( fontsize = 22,font = 8 ) ),
              
              tableGrob( range_captia,
                         theme = myt,
                         rows = NULL),
             spreadplt.new,
             rangeplt.new,
             nrow = 2)
```

![](Factor_and_figure_management_files/figure-markdown_github/remake%20plot-1.png)

**Changes I made**
\* for table:
+ remove the rownames + change the table theme

-   for boxplot:
    -   write the mean gdp/capita on each continent
    -   change the theme
        -   label size
        -   legend size
        -   backgroud
        -   border
        -   add "$" on y label
-   for line:
    -   change the theme
        -   label size
        -   legend size
        -   legend name and lengend item label size
        -   backgroud
        -   border
        -   add "$" on y label
-   add a title of these three graphs and change its font and size

plotly
------

``` r
# ggplotly(spreadplt.new)
```

ggplotly() can:

-   zoom in and zoom out
-   show the statistics summary of boxplot
-   can download the plot as png
-   [more details could be found here](https://plot.ly/r/)

Part 4: Writing figures to file
===============================

write figure
------------

``` r
ggsave("graph.png", graph, width=50, height=30, units = "cm")
```

load figure
-----------

![](https://github.com/STAT545-UBC-students/hw05-Sukeysun/blob/master/graph.png)

But I want to do more!
======================

``` r
country_language <- 
  data.frame(
  country = c("Afghanistan","Italy", "Germany", "Japan","Spain","Portugal"),
  language = c("Dari Persian","Italian","German","Japanese","Spanish","Portuguese")
) 
sub_gapminder <- gapminder %>%
  filter( country %in% c("Afghanistan","Italy", "Germany", "Japan","Spain","Portugal") ) 

subgapminder.add.language <- left_join(sub_gapminder,country_language)
```

    ## Joining, by = "country"

    ## Warning: Column `country` joining factors with different levels, coercing
    ## to character vector

``` r
## check the structure of subgapminder.add.language
data.frame( variable = names( subgapminder.add.language ),
           classes = sapply( subgapminder.add.language, class ),
           factorlevel = sapply( subgapminder.add.language, nlevels ),
           first_values = sapply( subgapminder.add.language, function( x ) paste0( head( x ),  collapse = ", ") ),
           row.names = NULL ) %>% 
  kable()
```

| variable  | classes   |  factorlevel| first\_values                                                                      |
|:----------|:----------|------------:|:-----------------------------------------------------------------------------------|
| country   | character |            0| Afghanistan, Afghanistan, Afghanistan, Afghanistan, Afghanistan, Afghanistan       |
| continent | factor    |            5| Asia, Asia, Asia, Asia, Asia, Asia                                                 |
| year      | integer   |            0| 1952, 1957, 1962, 1967, 1972, 1977                                                 |
| lifeExp   | numeric   |            0| 28.801, 30.332, 31.997, 34.02, 36.088, 38.438                                      |
| pop       | integer   |            0| 8425333, 9240934, 10267083, 11537966, 13079460, 14880372                           |
| gdpPercap | numeric   |            0| 779.4453145, 820.8530296, 853.10071, 836.1971382, 739.9811058, 786.11336           |
| language  | factor    |            6| Dari Persian, Dari Persian, Dari Persian, Dari Persian, Dari Persian, Dari Persian |

Country is character now. But we can make sure that both factors have the same levels before merging:

``` r
combined <- sort( union(levels(sub_gapminder$country), levels(country_language$country ) ) )

new.subgapminder.add.language<- left_join(
  mutate( sub_gapminder, country = factor( country, levels = combined ) ),
  mutate( country_language, country = factor( country, levels = combined ) )
  )
```

    ## Joining, by = "country"

``` r
## check the structure of new.subgapminder.add.language
data.frame( variable = names( new.subgapminder.add.language ),
           classes = sapply( new.subgapminder.add.language, class ),
           factorlevel = sapply( new.subgapminder.add.language, nlevels ),
           first_values = sapply( new.subgapminder.add.language, function( x ) paste0( head( x ),  collapse = ", ") ),
           row.names = NULL ) %>% 
  kable()
```

| variable  | classes |  factorlevel| first\_values                                                                      |
|:----------|:--------|------------:|:-----------------------------------------------------------------------------------|
| country   | factor  |          142| Afghanistan, Afghanistan, Afghanistan, Afghanistan, Afghanistan, Afghanistan       |
| continent | factor  |            5| Asia, Asia, Asia, Asia, Asia, Asia                                                 |
| year      | integer |            0| 1952, 1957, 1962, 1967, 1972, 1977                                                 |
| lifeExp   | numeric |            0| 28.801, 30.332, 31.997, 34.02, 36.088, 38.438                                      |
| pop       | integer |            0| 8425333, 9240934, 10267083, 11537966, 13079460, 14880372                           |
| gdpPercap | numeric |            0| 779.4453145, 820.8530296, 853.10071, 836.1971382, 739.9811058, 786.11336           |
| language  | factor  |            6| Dari Persian, Dari Persian, Dari Persian, Dari Persian, Dari Persian, Dari Persian |

Now country is a factor with 142 levels, but we only have 6 levels.

``` r
sub_gapminder <- gapminder %>%
  filter( country %in% c("Afghanistan","Italy", "Germany", "Japan","Spain","Portugal") ) %>% 
  droplevels()

subgapminder.add.language <- left_join(sub_gapminder,country_language)
```

    ## Joining, by = "country"

``` r
## check the structure of subgapminder.add.language
data.frame( variable = names( subgapminder.add.language ),
           classes = sapply( subgapminder.add.language, class ),
           factorlevel = sapply( subgapminder.add.language, nlevels ),
           first_values = sapply( subgapminder.add.language, function( x ) paste0( head( x ),  collapse = ", ") ),
           row.names = NULL ) %>% 
  kable()
```

| variable  | classes |  factorlevel| first\_values                                                                      |
|:----------|:--------|------------:|:-----------------------------------------------------------------------------------|
| country   | factor  |            6| Afghanistan, Afghanistan, Afghanistan, Afghanistan, Afghanistan, Afghanistan       |
| continent | factor  |            2| Asia, Asia, Asia, Asia, Asia, Asia                                                 |
| year      | integer |            0| 1952, 1957, 1962, 1967, 1972, 1977                                                 |
| lifeExp   | numeric |            0| 28.801, 30.332, 31.997, 34.02, 36.088, 38.438                                      |
| pop       | integer |            0| 8425333, 9240934, 10267083, 11537966, 13079460, 14880372                           |
| gdpPercap | numeric |            0| 779.4453145, 820.8530296, 853.10071, 836.1971382, 739.9811058, 786.11336           |
| language  | factor  |            6| Dari Persian, Dari Persian, Dari Persian, Dari Persian, Dari Persian, Dari Persian |
