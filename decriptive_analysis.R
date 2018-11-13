library(ggplot2)
library(dplyr)
library(naniar)
library(doParallel)
library(lattice)
library(RColorBrewer)

set.seed(42)

data <- read.csv("3-profiles.csv")
data <- data.frame(data)

data.without.essay <- dplyr::select(data,-one_of(c("essay1", "essay2", "essay3","essay4","essay5","essay6","essay7","essay8","essay9","essay0")))
data.cleaned <- replace_with_na(data.without.essay,replace = list(income = c(-1),body_type=c(""),job=c(""),ethnicity=c(""),
                                                                  sign=c(""),religion=c(""),pets=c(""),offspring=c(""),
                                                                  smokes=c(""), diet=c(""),drinks=c(""),education=c(""),drugs=c(""),speaks=c("")))

gg_miss_var(data.cleaned)
gg_miss_upset(data.cleaned, nsets=5,nintersects=20)

data.cleaned <- data.cleaned %>% mutate(religious_affil = gsub(" [A-z ]*", "", religion))
data.cleaned$religious_affil

men <- dplyr::filter(data.cleaned, sex == "m" )
female <- dplyr::filter(data.cleaned, sex == "f" )

## Religion and orientation


plot_religion_1 <- ggplot(data=subset(men, !is.na(religious_affil) & !is.na(drinks)), aes(religious_affil) ) +
    geom_bar() + facet_grid(drinks ~.)

scale_x_continuous(name="Religious affiliation", limits=c(0, 30)) +
    plot_religion_1 + labs(x = "Religious affiliation")
plot_religion_1 + labs(title = "Religion and drinking")
plot_religion_1+theme_minimal()
