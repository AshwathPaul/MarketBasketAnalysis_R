# MarketBasketAnalysis_R

## Market Basket Analysis including Visualization of Rules using R

Install the following packages before running the Markdown file
install.packages("ggplot")

install.packages("dplyr")

install.packages("arules")

install.packages("arulesViz")

The dataset Groceries is included as part of the arules package.

Alternatively, if you want to use your own data, read the dataset as transactions using the following line of code:

tr <- read.transactions("demo_basket", format = "basket", sep=",", skip = 1)
