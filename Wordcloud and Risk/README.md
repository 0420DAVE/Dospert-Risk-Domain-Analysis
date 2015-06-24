## Introduction to project US Hospital Comparator
## 
### Data Source
This shiny app is named US Hospital Comparater. It is used for direct
comparison among all US hospitals in terms of their mortality rate in
three domains: heart attack, heart failure, and pneumonia.

The data is downloaded at
https://d396qusza40orc.cloudfront.net/rprog%2Fdata%2FProgAssignment3-
data.zip

You find Hospital Compare data on data.medicare.gov. This website allows
you to view the data files embedded on a webpage without downloading
them. The data on data.medicare.gov can usually be viewed the same day
it has been updated on Hospital Compare. Use data.medicare.gov to
customize views and filter the Hospital Compare data.

### Data Visualization type
Visit this link to visualize wordcloud of people of different risk attitude (high vs low). For detailed instruction, read `about` in the app's panel bar: https://dave0420.shinyapps.io/shiny

![Alt text](https://github.com/0420DAVE/Dospert-Risk-Domain-Analysis/blob/master/Wordcloud%20and%20Risk/Screenshot%202015-06-24%2012.34.20.png?raw=true "ShinyApp Printscreen")


### App Instructions
On the left sidebar, select one type of domain you can to compare (heart
attack, heart failure, or pneumonia); also select the state you want to
include in comparison.

The table panel will return you a table consist of the hospitals that
match your search, arranging in ascedning order of mortality rate. So
the hospital comes in the first line has the lowest mortality rate. The
data visualization panel returns interactive graph that showes hospital
names and their mortality rate of given health domain. Note that in the
graph, mortality rate is adjusted to the average mean of all hospitals
in the state that you have selected.


© Li Ying 2015 All Rights reserved. ****
