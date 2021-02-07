Market Basket Analysis
================
Ashwath Paul
02 February 2021

Reading the data \#Install the required packages

``` r
library(arules)
```

    ## Warning: package 'arules' was built under R version 3.6.3

    ## Loading required package: Matrix

    ## 
    ## Attaching package: 'arules'

    ## The following objects are masked from 'package:base':
    ## 
    ##     abbreviate, write

``` r
#install.packages("arulesViz")
library(arulesViz)
```

    ## Warning: package 'arulesViz' was built under R version 3.6.3

    ## Loading required package: grid

``` r
library(dplyr)
```

    ## 
    ## Attaching package: 'dplyr'

    ## The following objects are masked from 'package:arules':
    ## 
    ##     intersect, recode, setdiff, setequal, union

    ## The following objects are masked from 'package:stats':
    ## 
    ##     filter, lag

    ## The following objects are masked from 'package:base':
    ## 
    ##     intersect, setdiff, setequal, union

``` r
library(ggplot2)
```

    ## Warning: package 'ggplot2' was built under R version 3.6.2

\#Read the data as transactions

``` r
data("Groceries")
summary(Groceries)
```

    ## transactions as itemMatrix in sparse format with
    ##  9835 rows (elements/itemsets/transactions) and
    ##  169 columns (items) and a density of 0.02609146 
    ## 
    ## most frequent items:
    ##       whole milk other vegetables       rolls/buns             soda 
    ##             2513             1903             1809             1715 
    ##           yogurt          (Other) 
    ##             1372            34055 
    ## 
    ## element (itemset/transaction) length distribution:
    ## sizes
    ##    1    2    3    4    5    6    7    8    9   10   11   12   13   14   15 
    ## 2159 1643 1299 1005  855  645  545  438  350  246  182  117   78   77   55 
    ##   16   17   18   19   20   21   22   23   24   26   27   28   29   32 
    ##   46   29   14   14    9   11    4    6    1    1    1    1    3    1 
    ## 
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##   1.000   2.000   3.000   4.409   6.000  32.000 
    ## 
    ## includes extended item information - examples:
    ##        labels  level2           level1
    ## 1 frankfurter sausage meat and sausage
    ## 2     sausage sausage meat and sausage
    ## 3  liver loaf sausage meat and sausage

``` r
g=summary(Groceries)
barplot(g@lengths,xlab = "Number of items per transaction", ylab= "Frequency")
```

![](MBA_files/figure-gfm/unnamed-chunk-2-1.png)<!-- -->

``` r
itemFrequency(Groceries[,1:10])
```

    ##       frankfurter           sausage        liver loaf               ham 
    ##       0.058973055       0.093950178       0.005083884       0.026029487 
    ##              meat finished products   organic sausage           chicken 
    ##       0.025826131       0.006507372       0.002236909       0.042907982 
    ##            turkey              pork 
    ##       0.008134215       0.057651246

\#Checking the item frequency

``` r
itemFrequencyPlot(Groceries, support = .10)
```

![](MBA_files/figure-gfm/unnamed-chunk-3-1.png)<!-- --> \#Fit
apriori

``` r
grocery_apriori <- apriori(Groceries, parameter=list(support=.02, confidence=.4, minlen=2))
```

    ## Apriori
    ## 
    ## Parameter specification:
    ##  confidence minval smax arem  aval originalSupport maxtime support minlen
    ##         0.4    0.1    1 none FALSE            TRUE       5    0.02      2
    ##  maxlen target  ext
    ##      10  rules TRUE
    ## 
    ## Algorithmic control:
    ##  filter tree heap memopt load sort verbose
    ##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
    ## 
    ## Absolute minimum support count: 196 
    ## 
    ## set item appearances ...[0 item(s)] done [0.00s].
    ## set transactions ...[169 item(s), 9835 transaction(s)] done [0.03s].
    ## sorting and recoding items ... [59 item(s)] done [0.00s].
    ## creating transaction tree ... done [0.01s].
    ## checking subsets of size 1 2 3 done [0.00s].
    ## writing ... [15 rule(s)] done [0.00s].
    ## creating S4 object  ... done [0.01s].

``` r
summary(grocery_apriori)
```

    ## set of 15 rules
    ## 
    ## rule length distribution (lhs + rhs):sizes
    ##  2  3 
    ## 12  3 
    ## 
    ##    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
    ##     2.0     2.0     2.0     2.2     2.0     3.0 
    ## 
    ## summary of quality measures:
    ##     support          confidence        coverage            lift      
    ##  Min.   :0.02044   Min.   :0.4016   Min.   :0.04342   Min.   :1.572  
    ##  1st Qu.:0.02318   1st Qu.:0.4091   1st Qu.:0.05069   1st Qu.:1.640  
    ##  Median :0.02755   Median :0.4487   Median :0.05857   Median :1.850  
    ##  Mean   :0.03159   Mean   :0.4480   Mean   :0.07178   Mean   :1.863  
    ##  3rd Qu.:0.03726   3rd Qu.:0.4816   3rd Qu.:0.08831   3rd Qu.:1.977  
    ##  Max.   :0.05602   Max.   :0.5129   Max.   :0.13950   Max.   :2.450  
    ##      count      
    ##  Min.   :201.0  
    ##  1st Qu.:228.0  
    ##  Median :271.0  
    ##  Mean   :310.7  
    ##  3rd Qu.:366.5  
    ##  Max.   :551.0  
    ## 
    ## mining info:
    ##       data ntransactions support confidence
    ##  Groceries          9835    0.02        0.4

\#View the
    rules

``` r
inspect(sort(grocery_apriori, by="lift"))
```

    ##      lhs                                   rhs                support   
    ## [1]  {root vegetables,whole milk}       => {other vegetables} 0.02318251
    ## [2]  {root vegetables}                  => {other vegetables} 0.04738180
    ## [3]  {whipped/sour cream}               => {other vegetables} 0.02887646
    ## [4]  {other vegetables,yogurt}          => {whole milk}       0.02226741
    ## [5]  {butter}                           => {whole milk}       0.02755465
    ## [6]  {curd}                             => {whole milk}       0.02613116
    ## [7]  {root vegetables,other vegetables} => {whole milk}       0.02318251
    ## [8]  {domestic eggs}                    => {whole milk}       0.02999492
    ## [9]  {whipped/sour cream}               => {whole milk}       0.03223183
    ## [10] {root vegetables}                  => {whole milk}       0.04890696
    ## [11] {frozen vegetables}                => {whole milk}       0.02043721
    ## [12] {margarine}                        => {whole milk}       0.02419929
    ## [13] {beef}                             => {whole milk}       0.02125064
    ## [14] {tropical fruit}                   => {whole milk}       0.04229792
    ## [15] {yogurt}                           => {whole milk}       0.05602440
    ##      confidence coverage   lift     count
    ## [1]  0.4740125  0.04890696 2.449770 228  
    ## [2]  0.4347015  0.10899847 2.246605 466  
    ## [3]  0.4028369  0.07168277 2.081924 284  
    ## [4]  0.5128806  0.04341637 2.007235 219  
    ## [5]  0.4972477  0.05541434 1.946053 271  
    ## [6]  0.4904580  0.05327911 1.919481 257  
    ## [7]  0.4892704  0.04738180 1.914833 228  
    ## [8]  0.4727564  0.06344687 1.850203 295  
    ## [9]  0.4496454  0.07168277 1.759754 317  
    ## [10] 0.4486940  0.10899847 1.756031 481  
    ## [11] 0.4249471  0.04809354 1.663094 201  
    ## [12] 0.4131944  0.05856634 1.617098 238  
    ## [13] 0.4050388  0.05246568 1.585180 209  
    ## [14] 0.4031008  0.10493137 1.577595 416  
    ## [15] 0.4016035  0.13950178 1.571735 551

``` r
fit<-sort(grocery_apriori,by="lift")
inspect(fit)
```

    ##      lhs                                   rhs                support   
    ## [1]  {root vegetables,whole milk}       => {other vegetables} 0.02318251
    ## [2]  {root vegetables}                  => {other vegetables} 0.04738180
    ## [3]  {whipped/sour cream}               => {other vegetables} 0.02887646
    ## [4]  {other vegetables,yogurt}          => {whole milk}       0.02226741
    ## [5]  {butter}                           => {whole milk}       0.02755465
    ## [6]  {curd}                             => {whole milk}       0.02613116
    ## [7]  {root vegetables,other vegetables} => {whole milk}       0.02318251
    ## [8]  {domestic eggs}                    => {whole milk}       0.02999492
    ## [9]  {whipped/sour cream}               => {whole milk}       0.03223183
    ## [10] {root vegetables}                  => {whole milk}       0.04890696
    ## [11] {frozen vegetables}                => {whole milk}       0.02043721
    ## [12] {margarine}                        => {whole milk}       0.02419929
    ## [13] {beef}                             => {whole milk}       0.02125064
    ## [14] {tropical fruit}                   => {whole milk}       0.04229792
    ## [15] {yogurt}                           => {whole milk}       0.05602440
    ##      confidence coverage   lift     count
    ## [1]  0.4740125  0.04890696 2.449770 228  
    ## [2]  0.4347015  0.10899847 2.246605 466  
    ## [3]  0.4028369  0.07168277 2.081924 284  
    ## [4]  0.5128806  0.04341637 2.007235 219  
    ## [5]  0.4972477  0.05541434 1.946053 271  
    ## [6]  0.4904580  0.05327911 1.919481 257  
    ## [7]  0.4892704  0.04738180 1.914833 228  
    ## [8]  0.4727564  0.06344687 1.850203 295  
    ## [9]  0.4496454  0.07168277 1.759754 317  
    ## [10] 0.4486940  0.10899847 1.756031 481  
    ## [11] 0.4249471  0.04809354 1.663094 201  
    ## [12] 0.4131944  0.05856634 1.617098 238  
    ## [13] 0.4050388  0.05246568 1.585180 209  
    ## [14] 0.4031008  0.10493137 1.577595 416  
    ## [15] 0.4016035  0.13950178 1.571735 551

``` r
inspect(fit[is.redundant(fit)])
fit <- fit[!is.redundant(fit)]
plot(fit)
```

![](MBA_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

``` r
plot(fit, measure = c("support", "lift"), shading = "confidence")
```

![](MBA_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

``` r
plot(fit,cex=1)
```

![](MBA_files/figure-gfm/unnamed-chunk-8-2.png)<!-- -->

``` r
plot(fit,method="graph")
```

![](MBA_files/figure-gfm/unnamed-chunk-8-3.png)<!-- -->

``` r
rules <- apriori(Groceries, parameter=list(support=0.001, confidence=0.5))
```

    ## Apriori
    ## 
    ## Parameter specification:
    ##  confidence minval smax arem  aval originalSupport maxtime support minlen
    ##         0.5    0.1    1 none FALSE            TRUE       5   0.001      1
    ##  maxlen target  ext
    ##      10  rules TRUE
    ## 
    ## Algorithmic control:
    ##  filter tree heap memopt load sort verbose
    ##     0.1 TRUE TRUE  FALSE TRUE    2    TRUE
    ## 
    ## Absolute minimum support count: 9 
    ## 
    ## set item appearances ...[0 item(s)] done [0.00s].
    ## set transactions ...[169 item(s), 9835 transaction(s)] done [0.01s].
    ## sorting and recoding items ... [157 item(s)] done [0.00s].
    ## creating transaction tree ... done [0.01s].
    ## checking subsets of size 1 2 3 4 5 6 done [0.03s].
    ## writing ... [5668 rule(s)] done [0.00s].
    ## creating S4 object  ... done [0.00s].

``` r
subrules2 <- head(rules, n = 12, by = "lift")
inspect(subrules2)
```

    ##      lhs                        rhs                  support confidence    coverage     lift count
    ## [1]  {Instant food products,                                                                      
    ##       soda}                  => {hamburger meat} 0.001220132  0.6315789 0.001931876 18.99565    12
    ## [2]  {soda,                                                                                       
    ##       popcorn}               => {salty snack}    0.001220132  0.6315789 0.001931876 16.69779    12
    ## [3]  {flour,                                                                                      
    ##       baking powder}         => {sugar}          0.001016777  0.5555556 0.001830198 16.40807    10
    ## [4]  {ham,                                                                                        
    ##       processed cheese}      => {white bread}    0.001931876  0.6333333 0.003050330 15.04549    19
    ## [5]  {whole milk,                                                                                 
    ##       Instant food products} => {hamburger meat} 0.001525165  0.5000000 0.003050330 15.03823    15
    ## [6]  {other vegetables,                                                                           
    ##       curd,                                                                                       
    ##       yogurt,                                                                                     
    ##       whipped/sour cream}    => {cream cheese }  0.001016777  0.5882353 0.001728521 14.83409    10
    ## [7]  {processed cheese,                                                                           
    ##       domestic eggs}         => {white bread}    0.001118454  0.5238095 0.002135231 12.44364    11
    ## [8]  {tropical fruit,                                                                             
    ##       other vegetables,                                                                           
    ##       yogurt,                                                                                     
    ##       white bread}           => {butter}         0.001016777  0.6666667 0.001525165 12.03058    10
    ## [9]  {hamburger meat,                                                                             
    ##       yogurt,                                                                                     
    ##       whipped/sour cream}    => {butter}         0.001016777  0.6250000 0.001626843 11.27867    10
    ## [10] {tropical fruit,                                                                             
    ##       other vegetables,                                                                           
    ##       whole milk,                                                                                 
    ##       yogurt,                                                                                     
    ##       domestic eggs}         => {butter}         0.001016777  0.6250000 0.001626843 11.27867    10
    ## [11] {liquor,                                                                                     
    ##       red/blush wine}        => {bottled beer}   0.001931876  0.9047619 0.002135231 11.23527    19
    ## [12] {other vegetables,                                                                           
    ##       yogurt,                                                                                     
    ##       whipped/sour cream,                                                                         
    ##       cream cheese }         => {curd}           0.001016777  0.5882353 0.001728521 11.04064    10

``` r
plot(subrules2,method="graph")
```

![](MBA_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

``` r
plot(subrules2, method = "paracoord")
```

![](MBA_files/figure-gfm/unnamed-chunk-9-2.png)<!-- -->

``` r
plot(rules, method = "two-key plot",jitter=0)
```

![](MBA_files/figure-gfm/unnamed-chunk-9-3.png)<!-- -->

``` r
plot(rules, method = "grouped",control = list(k = 10))
```

![](MBA_files/figure-gfm/unnamed-chunk-9-4.png)<!-- -->
