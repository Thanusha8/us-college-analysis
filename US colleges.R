install.packages("ISLR2")

library(tidyverse)
library(ISLR2)


?College
glimpse(College)
view(College)

# Exploratory graphics

ggplot(College , aes(x=Grad.Rate)) +
  geom_histogram()   #note weird outlier

suspicious <- filter(College, Grad.Rate >= 100)
view(suspicious)



ggplot(College, aes(x= log10(F.Undergrad),
                    y= Grad.Rate)) +
  geom_point()



College_sm <- College %>%
  mutate(log_full = log10(F.Undergrad)) %>%
  select(Grad.Rate,
         log_full,
         Private,
         Top25perc)

View(College_sm)



# Modeling

ggplot(College, aes(x= log10(F.Undergrad),
                    y= Grad.Rate)) +
  geom_point() +
  geom_smooth(method="lm")


model_undergrad  <- lm(Grad.Rate ~ log_full,
                       data = College_sm)
summary(model_undergrad)
plot(model_undergrad)





#  What about Private?

ggplot(College, aes(x= log10(F.Undergrad),
                    y= Grad.Rate,
                    colour = Private)) +
  geom_point() +
  geom_smooth(method="lm",
              se = FALSE) +
  scale_color_brewer(palette = "Dark2")

model_private <- lm(Grad.Rate ~ Private + log_full,
                    data = College_sm)
summary(model_private)




# Interaction: private and F.Undergrad


model_private_int <- lm(Grad.Rate ~ Private * log_full,
                        data = College_sm)
summary(model_private_int)
anova(model_private_int)



# What about Top25perc?

model_top <- lm(Grad.Rate ~ Private + 
                  log_full +
                  Top25perc,
                data = College_sm)
summary(model_top)
plot(model_top)











