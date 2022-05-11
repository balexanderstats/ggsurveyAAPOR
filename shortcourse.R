## ----echo=FALSE, warning = FALSE, message=FALSE-------------------------------
knitr::opts_chunk$set(warning = FALSE, message = FALSE, fig.width = 6, fig.asp = .5, out.width = "95%",tidy.opts=list(width.cutoff=30), tidy = TRUE)
knitr::knit_hooks$set(purl = knitr::hook_purl)

## ----message=FALSE------------------------------------------------------------
library(ggplot2)
plot = ggplot(mtcars, aes(x = hp, y = mpg))+geom_point()
plot

## ----message=FALSE------------------------------------------------------------
plotnew = plot + ggtitle("MPG of Car given HP by
                         transmission") + xlab("Horse Power") + ylab("Miles Per Gallon")+facet_grid(~am)

## ----message=FALSE------------------------------------------------------------
plotnew

## -----------------------------------------------------------------------------
library(survey)
data(api)
library(dplyr)

## -----------------------------------------------------------------------------
ggplot(apistrat, aes(yr.rnd))+
  geom_bar(aes(weight = pw))+
  facet_grid(cols = vars(stype))

## -----------------------------------------------------------------------------
newdf = apistrat %>% group_by(stype, yr.rnd)%>%
  tally(, wt = pw) %>% mutate(f = n/sum(n))
plotnew = ggplot(newdf, aes(yr.rnd)) +
  geom_bar(aes(weight = f))+
  facet_grid(cols = vars(stype))

## -----------------------------------------------------------------------------
plotnew

## -----------------------------------------------------------------------------
library(ggsurvey)
ggbarcrosstabs(apistrat, yr.rnd, stype, pw)

## -----------------------------------------------------------------------------
library(palmerpenguins)
data(penguins)

## -----------------------------------------------------------------------------
plot3 = ggplot(penguins, aes(species)) + geom_bar()
plot3

## -----------------------------------------------------------------------------
plot3+ggtitle("Species of Penguins")

## -----------------------------------------------------------------------------
plot3+ggtitle("Species of Penguins")+xlab("Species")

## -----------------------------------------------------------------------------
plot3title = plot3+ggtitle("Species of Penguins")+xlab("Species")+ylab("Number of Penguins")
plot3title

## -----------------------------------------------------------------------------
plot3title

## -----------------------------------------------------------------------------
plot3color = ggplot(penguins, aes(species)) + geom_bar(aes(fill = species))+ggtitle("Species of Penguins")+xlab("Species")+ylab("Number of Penguins")

## -----------------------------------------------------------------------------
plot3color

## ----message=FALSE------------------------------------------------------------
plotnew = plot + ggtitle("MPG of Car given HP by
                         transmission") + xlab("Horse Power") + ylab("Miles Per Gallon")+facet_grid(~am)

## -----------------------------------------------------------------------------
plot4  = ggplot(penguins, aes(species))+geom_bar() +facet_grid(year~island)
plot4

## -----------------------------------------------------------------------------
plot4

## -----------------------------------------------------------------------------
plot4a  = ggplot(penguins, aes(species))+geom_bar() +facet_grid(rows = vars(year), cols = vars(island))

## -----------------------------------------------------------------------------
plot4a

## -----------------------------------------------------------------------------
library(ggrepel)
plot4+theme(axis.text.x = element_text(angle = 45, hjust = 1))

## -----------------------------------------------------------------------------
plot4  = ggplot(penguins, aes(species))+geom_bar() +facet_wrap(vars(year,island))

## -----------------------------------------------------------------------------
plot4

## -----------------------------------------------------------------------------
plot5 =ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm))+geom_point()

## -----------------------------------------------------------------------------
plot5

## -----------------------------------------------------------------------------
plot5a = ggplot(penguins, aes(x = bill_length_mm, y = bill_depth_mm))+geom_point(aes(color = species, shape = sex))

## -----------------------------------------------------------------------------
plot5a

## -----------------------------------------------------------------------------
ggplot(penguins, aes(x = body_mass_g))+geom_histogram()

## -----------------------------------------------------------------------------
plot6b =ggplot(penguins, aes(x = body_mass_g))+geom_histogram(binwidth = 200)

## -----------------------------------------------------------------------------
plot6b

## -----------------------------------------------------------------------------
ggplot(penguins, aes(x = flipper_length_mm)) + geom_boxplot()

## -----------------------------------------------------------------------------
plot7 = ggplot(penguins, aes(x = flipper_length_mm, y = species)) + geom_boxplot()

## -----------------------------------------------------------------------------
plot7

## -----------------------------------------------------------------------------
plot7new = plot7 + ggtitle("Boxplot of Flipper Length") + xlab("Flipper Length")  + ylab("Species")
plot7new

## -----------------------------------------------------------------------------
plot7new

## -----------------------------------------------------------------------------
plot7new + facet_grid(~year)

## -----------------------------------------------------------------------------
#install.packages("devtools")
#library(devtools)
#install_github("jamesmartherus/anesr")
library(anesr)
data("timeseries_2020")
timeseries_2020 = haven::as_factor(timeseries_2020)

## -----------------------------------------------------------------------------
timeseries_2020$rightwrongtrack = timeseries_2020$V201114
timeseries_2020$speedcovid = timeseries_2020$V201390
timeseries_2020$economy = timeseries_2020$V201324
timeseries_2020$race = timeseries_2020$V201549x
timeseries_2020$gender = timeseries_2020$V201600
timeseries_2020 = droplevels(timeseries_2020)

## ----tidy=TRUE----------------------------------------------------------------
timeseries_2020 = timeseries_2020 %>% filter(race != "-9. Refused",
  race != "-8. Don't know", gender != "-9. Refused",
  rightwrongtrack != "-9. Refused",
  rightwrongtrack != "-8. Don't know",
  economy != "-9. Refused", speedcovid !="-9. Refused")
timeseries_2020 = droplevels(timeseries_2020)

## -----------------------------------------------------------------------------
library(srvyr)
anes_survey  = svydesign(id=~V200001  ,
    strata = ~V200010d, weights = ~V200010a,
    data  = timeseries_2020)
anes_svy = as_survey(anes_survey)

## -----------------------------------------------------------------------------
plot8a = ggbarweight(timeseries_2020, rightwrongtrack, V200010a )

## -----------------------------------------------------------------------------
plot8a

## -----------------------------------------------------------------------------
plot8atitle = plot8a + ggtitle("Right Direction / Wrong Track") + ylab("Proportion")

## -----------------------------------------------------------------------------
plot8atitle

## -----------------------------------------------------------------------------
plot8b = ggbarweight_svy(anes_survey, rightwrongtrack)
plot8b

## -----------------------------------------------------------------------------
plot8title = plot8b + ggtitle("Right Direction / Wrong Track") + xlab("Proportion")

## -----------------------------------------------------------------------------
plot8title

## -----------------------------------------------------------------------------
filterplot = timeseries_2020 %>% filter(race == "1. White, non-Hispanic") %>%
  ggbarweight(rightwrongtrack, V200010a)

## -----------------------------------------------------------------------------
filterplot

## -----------------------------------------------------------------------------
ggbarcrosstabs(timeseries_2020, rightwrongtrack, gender, V200010a)

## -----------------------------------------------------------------------------
library(stringr)
plot9Atext = ggbarcrosstabs(timeseries_2020, rightwrongtrack, gender, V200010a) +   scale_x_discrete(labels = function(x)
  str_wrap(x, width = 10))

## -----------------------------------------------------------------------------
library(stringr)
plot9Atext

## -----------------------------------------------------------------------------
ggbarcrosstabs_svy(anes_survey, rightwrongtrack, gender)

## -----------------------------------------------------------------------------
ggbarcrosstabs_svy(anes_survey, rightwrongtrack, gender) +   scale_x_discrete(labels = function(x) str_wrap(x, width = 10))

## -----------------------------------------------------------------------------
plot10A = ggbarcrosstabs3d(timeseries_2020, rightwrongtrack,economy, gender, V200010a)

## -----------------------------------------------------------------------------
plot10A

## ----tidy=FALSE---------------------------------------------------------------
plot10Atext = ggbarcrosstabs3d(timeseries_2020,
rightwrongtrack,economy,gender, V200010a,
labeller = label_wrap_gen(width = 3, multi_line = TRUE)) +
  scale_x_discrete(labels = function(x)
    str_wrap(x, width = 10))

## -----------------------------------------------------------------------------
plot10Atext

## -----------------------------------------------------------------------------
ggbarcrosstabs3d_svy(anes_survey, rightwrongtrack, economy, gender)

## ----tidy=FALSE---------------------------------------------------------------
ggbarcrosstabs3d_svy(anes_survey, rightwrongtrack, economy
, gender,  labeller =
  label_wrap_gen(width = 3, multi_line = TRUE)) +
  scale_x_discrete(labels = function(x)
    str_wrap(x, width = 3))

## ----tidy=FALSE---------------------------------------------------------------
timeseries_2020$BidenTherm = as.numeric(as.character(
  timeseries_2020$V201151))
timeseries_2020$TrumpTherm = as.numeric(as.character(
  timeseries_2020$V201152))
anes_svy  = anes_svy %>% mutate(BidenTherm =
as.numeric(as.character(timeseries_2020$V201151)))
anes_svy  = anes_svy %>% mutate(TrumpTherm =
as.numeric(as.character(timeseries_2020$V201152)))

## -----------------------------------------------------------------------------
ggboxweight(timeseries_2020, BidenTherm, V200010a)

## -----------------------------------------------------------------------------
ggboxweight_svy(anes_svy, BidenTherm)

## -----------------------------------------------------------------------------
ggboxweight2d(timeseries_2020, BidenTherm, gender, V200010a)

## -----------------------------------------------------------------------------
ggboxweight2d_svy(anes_svy, BidenTherm, gender)+ggtitle("Biden Feeling Thermometer by Gender")

## -----------------------------------------------------------------------------
ggboxweight3d(timeseries_2020, BidenTherm, rightwrongtrack, gender, V200010a)

## -----------------------------------------------------------------------------
ggboxweight3d_svy(anes_svy, BidenTherm,rightwrongtrack, gender)+ggtitle("Biden Feeling Thermometer by Gender")

## -----------------------------------------------------------------------------
ggplot(timeseries_2020, aes(x = rightwrongtrack, y = BidenTherm))+geom_boxplot(aes(weight = V200010a))+facet_grid(rows = vars(gender))

## -----------------------------------------------------------------------------
gghistweight(timeseries_2020, TrumpTherm, V200010a)

## -----------------------------------------------------------------------------
gghistweight(timeseries_2020, TrumpTherm, V200010a, binwidth = 5)

## -----------------------------------------------------------------------------
gghistweight(timeseries_2020, TrumpTherm, V200010a, binwidth = 10)

## -----------------------------------------------------------------------------
gghistweight_svy(anes_svy, TrumpTherm, binwidth = 10)

## -----------------------------------------------------------------------------
gghistweight2d(timeseries_2020, TrumpTherm, gender, V200010a, binwidth = 10)

## -----------------------------------------------------------------------------
gghistweight2d_svy(anes_svy, TrumpTherm, gender, binwidth = 10)+ggtitle("Biden Feeling Thermometer by Gender")

## -----------------------------------------------------------------------------
gghistweight3d(timeseries_2020, TrumpTherm, rightwrongtrack, gender, V200010a, binwidth = 10)

## -----------------------------------------------------------------------------
gghistweight3d_svy(anes_svy, TrumpTherm,rightwrongtrack, gender, binwidth = 10)

## -----------------------------------------------------------------------------
ggplot(timeseries_2020, aes(TrumpTherm))+geom_histogram(aes(weight = V200010a), binwidth = 10)+facet_grid(rows = vars(gender))

## -----------------------------------------------------------------------------
ggplot(timeseries_2020, aes(x = BidenTherm, y = TrumpTherm))+geom_point()
nrow(timeseries_2020)

## -----------------------------------------------------------------------------
library(hexbin)
ggplot(timeseries_2020, aes(x = BidenTherm, y = TrumpTherm))+geom_hex()

## -----------------------------------------------------------------------------
gghexweight(timeseries_2020, BidenTherm, TrumpTherm, weight = V200010a )

## -----------------------------------------------------------------------------
gghexweight_svy(anes_svy, BidenTherm, TrumpTherm)

## -----------------------------------------------------------------------------
gghexweight2d(timeseries_2020, BidenTherm, TrumpTherm, gender, weight = V200010a)

## -----------------------------------------------------------------------------
gghexweight2d_svy(anes_svy, BidenTherm, TrumpTherm, gender)

## -----------------------------------------------------------------------------
gghexweight3d(timeseries_2020, BidenTherm, TrumpTherm, gender, rightwrongtrack, weight = V200010a)

## -----------------------------------------------------------------------------
gghexweight3d_svy(anes_svy, BidenTherm, TrumpTherm, gender, rightwrongtrack)

