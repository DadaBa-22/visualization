### INTRODUCTION TO GGPLOT2 FOR DATA VISUALIZATION


library(ggplot2)
  colnames(data)
ggplot(data, mapping = aes(x = radius_mean, y = perimeter_mean))

### I have to specify the type of graphy suitable for this two variable
ggplot(data, mapping = aes(x = radius_mean, y = perimeter_mean)) + geom_point()

### now i want to add color  and the line of best fit the plot
ggplot(data, mapping = aes(x = radius_mean, y = perimeter_mean)) +
  geom_point(color = "blue", alpha = 0.7, size = 3) +
  geom_smooth(method = "lm")

### Now we want to specifies the color to a specific variable
ggplot(data, mapping = aes(x = radius_mean, y = perimeter_mean, color = diagnosis)) + 
  geom_point(alpha = 0.7, size = 3,) + 
  geom_smooth(method = "lm", se = F)

ggplot(data,
       mapping = aes(x = radius_mean,
                     y = perimeter_mean, 
                     color = diagnosis)) +
  geom_point(alpha = .7,
             size = 3) +
  geom_smooth(method = "lm",
              se = FALSE, 
              size = 1.5) +
  scale_x_continuous(breaks = seq(0, 60, 10)) + 
  scale_y_continuous(breaks = seq(0, 30, 5),
                     label = scales::dollar) +
  scale_color_manual(values = c("indianred3",
                                            "cornflowerblue"))

### GGPLOT FOR CATEGORICAL
ggplot(data, aes(x = diagnosis)) +
  geom_bar(fill = "cornflowerblue", color="blue") +
  labs(x = "Diagnosis",
       y = "Frequency",
       title = "Patients Diagnosis")
prop.table(data$diagnosis)
table(data$diagnosis)
prop.table(table(data$diagnosis))

### When the you want to change the y axis to percentage
ggplot(data, aes(x = diagnosis,
           y = ..count.. / sum(..count..))) +
  geom_bar(fill = "cornflowerblue", color = "blue") +
  labs(x = "Diagnosis", 
       y = "Percent",
       title = "Patients Diagnosis") +
  scale_y_continuous(labels = scales::percent)
library(dplyr)
### Deriving a frequency table for diagnosis
bob<-data%>%
  count(diagnosis)
bob
### Graph of diagnosis

ggplot(bob, aes(x = diagnosis)) + 
  geom_bar(fill = "cornflowerblue", color = "blue") + 
  labs(x = "Diagnosis", y = "Frequency")

### Arranging the graph in ascending or descending order

ggplot(bob, aes(x = reorder(diagnosis, -n))) + 
  geom_bar(fill = "cornflowerblue", color = "blue") + 
  labs(x = "Diagnosi", y = "Frequency")

ggplot(bob,
       aes(x = reorder(diagnosis, n), 
           y = n)) +
  geom_bar(stat = "identity") + 
  labs(x = "Diagnosi",
       y = "Frequency",
       title = "Patients Diagnosis")


ggplot(bob,
       aes(x = reorder(diagnosis, n), 
           y = n)) +
  geom_bar(stat = "identity", fill = "cornflowerblue", color = "blue") + 
  labs(x = "Diagnosi",
       y = "Frequency",
       title = "Patients Diagnosis")
ggplot(bob,
       aes(x = reorder(diagnosis, n), 
           y = n/sum(n))) +
  geom_bar(stat = "identity", fill = "cornflowerblue", color = "blue") + 
  labs(x = "Diagnosi",
       y = "Frequency",
       title = "Patients Diagnosis") + 
  scale_y_continuous(labels = scales::percent)

###  Now i want to display the percentages on the bars
ggplot(bob,
       aes(x = reorder(diagnosis, n), 
           y = n/sum(n))) +
  geom_bar(stat = "identity", fill = "cornflowerblue", color = "blue") + 
  labs(x = "Diagnosi",
       y = "Frequency",
       title = "Patients Diagnosis") + 
  geom_text(aes(label = n/sum(n), ),
            vjust = -0.25) + 
  scale_y_continuous(labels = scales::percent)


ggplot(bob,
       aes(x = reorder(diagnosis, n), 
           y = n/sum(n))) +
  geom_bar(stat = "identity", fill = "cornflowerblue", color = "blue") + 
  labs(x = "Diagnosi",
       y = "Frequency",
       title = "Patients Diagnosis") + 
  geom_text(aes(label = n/sum(n)*100),
            vjust = -0.25) + 
  scale_y_continuous(labels = scales::percent) + 
  theme(axis.text.x = element_text(angle = 45,
                                   hjust = 1))

# bar chart with staggered labels
lbls <- paste0(c("", "\n"),
               levels(data$diagnosis))
ggplot(data,
       aes(x=factor(Diagnosis,
                    labels = lbls))) +
  geom_bar() +
  labs(x = "",
       y = "Frequency",
       title = "Patient Diagnosis")

# Create a basic ggplot2 Pie chart 
library(dplyr)
plotdata <- data %>%
  count(diagnosis) %>% 
  arrange(desc(diagnosis)) %>%
  mutate(prop = round(n * 100 / sum(n), 1), 
         lab.ypos = cumsum(prop) - 0.5  *prop)

plotdata


ggplot(plotdata,
       aes(x = "",
           y = prop,
           fill = diagnosis)) +
  geom_bar(width = 1,
           stat = "identity", 
           color = "black") +
  coord_polar("y",
              start = 0, 
              direction = -1) +
  theme_void()

# create a pie chart with slice labels 
plotdata <- data %>%
  count(diagnosis) %>% 
  arrange(desc(diagnosis)) %>%
  mutate(prop = round(n*100/sum(n), 1),
         lab.ypos = cumsum(prop) - 0.5*prop)

plotdata$label <- paste0(plotdata$diagnosis, "\n",
                         round(plotdata$prop), "%")
ggplot(plotdata,
       aes(x = "",
           y = prop,
           fill = diagnosis)) +
  geom_bar(width = 1,
           stat = "identity", 
           color = "black") +
  geom_text(aes(y = lab.ypos, label = label),
            color = "black") +
  coord_polar("y",
              start = 0, 
              direction = -1) +
  theme_void() +
  theme(legend.position = "FALSE") + 
  labs(title = "Patient Diagnosi")

### How to use ggplot2 for Tree map

### MULTIVARIET VISUALIZATION
library(carData)
Salaries
ggplot(Salaries, aes(x = yrs.since.phd, 
                     y = salary, color = rank,
                     size = yrs.service)) + 
  geom_point(alpha = .6) +
  labs(title = "Academic salary by rank, years of service, and years since degree")

# plot experience vs. salary (color represents rank) 
ggplot(Salaries, aes(x = yrs.since.phd,
                     y = salary, 
                     color=rank)) +
  geom_point() +
  labs(title = "Academic salary by rank and years since degree", )

### plot experience vs. salary with 
### fit lines (color represents sex) 
ggplot(Salaries,
       aes(x = yrs.since.phd, 
           y = salary,
           color = sex)) +
  geom_point(alpha = .4,
             size = 3) +
  geom_smooth(se=FALSE,
              method = "lm", 
              formula = y~poly(x,2), 
              size = 1.5) +
  labs(x = "Years Since Ph.D.",
       title = "Academic Salary by Sex and Years Experience", 
       subtitle = "9-month salary for 2008-2009",
       y = "",
       color = "Sex") +
  scale_y_continuous(label = scales::dollar) +
  scale_color_brewer(palette = "Set1") + 
  theme_minimal()

### plot salary histograms by rank 
ggplot(Salaries, aes(x = salary)) +
  geom_histogram(fill = "cornflowerblue",
                 color = "white") +
  facet_wrap(~rank, ncol = 1) +
  labs(title = "Salary histograms by rank")


### plot salary histograms by rank and sex 
ggplot(Salaries, aes(x = salary / 1000)) +
  geom_histogram(color = "white",
                 fill = "cornflowerblue") +
  facet_grid(sex ~ rank) +
  labs(title = "Salary histograms  sex and rank", 
       x = "Salary ($1000)")

### Building an interactive Graph
# create leaflet graph 
library(leaflet) 
leaflet() %>%
  addTiles() %>%
  addMarkers(lng=-72.6560002,
             lat=41.5541829,
             popup="The birthplace of quantitative wisdom.</br> 
No, Waldo is not here.")

library(plotly)
p <- ggplot(mpg, aes(x=displ,
                     y=hwy,
                     color=class)) +
  geom_point(size=3) +
  labs(x = "Engine displacement", 
       y = "Highway Mileage", 
       color = "Car Class") +
  theme_bw()
ggplotly(p)

# create rbokeh graph 
# prepare data
data(mtcars)
View(mtcars)
mtcars$name <- row.names(mtcars) 
mtcars$cyl <- factor(mtcars$cyl)
# graph it 
library(rbokeh) 
figure() %>%
  ly_points(disp, mpg, data=mtcars,
            color = cyl, glyph = cyl, 
            hover = list(name, mpg, wt))


# create interactive bar chart 
library(rCharts)
hair_eye_male = subset(as.data.frame(HairEyeColor),
                       Sex == "Male")
hair_eye_male
n1 <- nPlot(Freq ~ Hair,
            group = 'Eye',
            data = hair_eye_male, 
            type = 'multiBarChart'
)
n1$set(width = 600)
n1$show('iframesrc', cdn=TRUE)

### GENERATING A SURVIVAL CURVE
# plot survival curve 
library(survival) 
library(survminer)
data(lung)
sfit <- survfit(Surv(time, status) ~ sex, data=lung) 
ggsurvplot(sfit,
           title="Kaplan-Meier curve for lung cancer survival")
ggsurvplot(sfit,
           conf.intf = F, 
           pval=TRUE,
           legend.labs = c("Male", "Female"), 
           legend.title="Sex",
           palette=c("cornflowerblue", "indianred3"),
           title="Kaplan-Meier Curve for lung cancer survival", 
           xlab = "Time (days)")



