cupid <- cupid_data
#sapply(cupid, function(x) sum(is.na(x))) #find the missings per column
#summary(cupid)
#summary(cupid[which(cupid$sex=="f"),])
#summary(cupid[which(cupid$sex=="m"),])

#First plot, number of people per sex
sex_dist <- ggplot(cupid, aes(x = sex, fill= sex)) + 
  geom_bar(stat = "count") +
  ylab("Number of people") +
  xlab("Sex") + theme_minimal() + 
  scale_y_continuous(breaks = seq(0, 35000, by = 5000)) +
  scale_fill_discrete("Sex")

#Density plot, plot 
cdat <- ddply(cupid, "sex", summarise, age.mean=mean(age))
#cdat
dens <- ggplot(cupid, aes(x = age, fill = sex)) + geom_density(alpha=0.5) + xlab("Age") + ylab("Density")+ geom_vline(data=cdat, aes(xintercept=age.mean,  colour=sex),
                                                                                                                  linetype="dashed", size=1) + scale_x_continuous(breaks = seq(20, 90, by = 5),"Age", limits = c(18,92)) + theme_minimal()
#plot(dens)

#Did not use the following
# p_man <- ggplot(cupid[which(cupid$sex=="m"),], aes(age)) + geom_histogram(bins=50)
# plot(p_man)
# p_woman <- ggplot(cupid[which(cupid$sex=="f"),], aes(age)) + geom_histogram(bins=50)
# plot(p_woman)
# p_sex <- ggplot(cupid, aes(x=age, fill= sex)) + geom_histogram(bins = 40, position = "identity", alpha=0.5)
# print(p_sex)
# box <- ggplot(cupid, aes(sex, age, fill=sex)) +
#   geom_boxplot(alpha = 0.5)
# plot(box)

#Number of people by age, plot 3
age_cupid <- cupid[,c("age", "sex")]
count <- as.data.frame(table(age_cupid))
count$age <- as.numeric(as.character(count$age))
#diff.sex <- as.data.frame(count[55:108,3] - count[1:54,3])
#diff.sex <- cbind(c(18:69, 109, 110), rep("d", 54), diff.sex)
#names(diff.sex) <- c("age", "sex", "Freq")
#count <- rbind(count, diff.sex)
age_graph <- ggplot(count, aes(x= age, y = Freq, colour = sex)) + 
  geom_line(size = 1.5)  + ylab("Number of people") + ggtitle("Number of people by age") + scale_x_continuous(breaks = seq(20, 90, by = 5),"Age") + theme_minimal() + coord_cartesian(xlim=c(18, 92))

#Age categories
# cupid$agecat <- NA
# cupid$agecat[cupid$age > 17 & cupid$age <= 21] <- "18-21"
# cupid$agecat[cupid$age > 21 & cupid$age <= 25] <- "22-25"
# cupid$agecat[cupid$age > 25 & cupid$age <= 29] <- "26-29"
# cupid$agecat[cupid$age > 29 & cupid$age <= 33] <- "30-33"
# cupid$agecat[cupid$age > 33 & cupid$age <= 37] <- "34-37"
# cupid$agecat[cupid$age > 37 & cupid$age <= 41] <- "38-41"
# cupid$agecat[cupid$age > 41 & cupid$age <= 45] <- "42-45"
# cupid$agecat[cupid$age > 45 & cupid$age <= 49] <- "46-49"
# cupid$agecat[cupid$age > 49 & cupid$age <= 53] <- "50-53"
# cupid$agecat[cupid$age > 53 & cupid$age <= 57] <- "54-57"
# cupid$agecat[cupid$age > 57 & cupid$age <= 61] <- "58-61"
# cupid$agecat[cupid$age > 61 & cupid$age <= 65] <- "62-65"
# cupid$agecat[cupid$age > 65 & cupid$age <= 69] <- "66-69"
# cupid$agecat[cupid$age > 69] <- "70+"
# cupid$agecat <- as.factor(cupid$agecat)

#Plot percentage of sex in age category, not used in paper!
# cupid %>%  dplyr::count(sex, agecat) %>% group_by(sex) %>% 
#   mutate(freq = n/sum(n)) %>% 
#   ggplot(aes(x = agecat, y = freq, fill = sex)) + 
#   geom_bar(stat="identity", position = 'dodge')


cupid$agecat <- factor(cupid$agecat, levels = rev(levels(cupid$agecat)))
#Plot proportion age categories, plot 4
proportion_age <- 
  ggplot(cupid, aes(agecat, fill = sex)) + geom_bar(position = 'fill') + coord_flip() + theme_minimal() + scale_y_continuous(breaks=seq(0,1,by=0.25), labels=c("0%", "25%", "50%", "75%", "100%"), "Percentage (%)") + xlab("Age categories") + geom_hline(yintercept=0.5, linetype = "dashed", size=0.5)

#Mosaic plot for orientation, plot 5

mosaic_plot <- 
  ggplot(cupid) + geom_mosaic(aes(x = product(sex), fill=orientation)) + xlab("Sex") + ylab("Orientation") + theme_minimal() + scale_fill_discrete(name = "Orientation")

#Income, plot 6
cupid$income[cupid$income==-1] <- NA
clean.income <- na.omit(cupid, cols="income")
analyzed <- clean.income %>% mutate(incomegroup = 10000 * income%/%10000)
analyzed$incomegroup <- analyzed$incomegroup %>% as.factor
analyzed$incomegroup <- factor(analyzed$incomegroup, levels = rev(levels(analyzed$incomegroup)))
analyzed$incomegroup %>% unique %>% sort
n.color <- length(unique(analyzed$incomegroup))
getPalette = colorRampPalette(brewer.pal(4, "Dark2"))
gender_income <- ggplot(analyzed, aes(sex, fill = incomegroup)) + geom_bar(position = 'fill') + scale_fill_manual(values = getPalette(n.color), guide = guide_legend(reverse=TRUE), name = "Income group", labels=c("1000000", "500000", "250000", "150000", "100000", "80000", "70000", "60000", "50000", "40000", "30000", "20000")) + coord_flip() + xlab("Sex") + ylab("Percentage (%)") + scale_y_continuous(breaks=seq(0,1,by=0.25), labels=c("0%", "25%", "50%", "75%", "100%")) + theme_minimal()

#Ethnicity, plot 7
clean.eth <- na.omit(cupid, cols="ethnicity")
clean.eth$ethnicity <- word(clean.eth$ethnicity,1)
clean.eth$ethnicity <- gsub(",", "", clean.eth$ethnicity)
clean.eth$ethnicity <- as.factor(clean.eth$ethnicity)
n.color <- length(unique(clean.eth$ethnicity))
getPalette = colorRampPalette(brewer.pal(4, "Dark2"))
gender_ethnicity <- 
  ggplot(clean.eth, aes(sex, fill = ethnicity)) + geom_bar(position = 'fill') + scale_fill_manual(values = getPalette(n.color), guide = guide_legend(reverse=TRUE) , name = "Ethnicity", labels=c("asian", "black", "hispanic/latin", "indian", "middle eastern", "native american", "other", "pacific islander", "white")) + coord_flip() + xlab("Sex") + ylab("Percentage (%)") + scale_y_continuous(breaks=seq(0,1,by=0.25), labels=c("0%", "25%", "50%", "75%", "100%")) + theme_minimal()

#Status, plot 8
cupid.status <- matrix(c("available", "married", "seeing someone", "single", "available", "married", "seeing someone", "single", "f", "f", "f", "f", "m", "m", "m", "m"), nrow = 8, ncol=2)
value <- c(0.027, 0.006, 0.045, 0.926, 0.034, 0.005, 0.030, 0.932)
yval <- c("2.7%", "0.6%", "4.5%", "92.6%", "3.4%", "0.5%", "3%", "93.2%")
cupid.status <- data.frame(cbind(cupid.status, as.integer(value), yval))
gender_status <- ggplot(cupid.status, aes(x = V1, y = value, fill = factor(V2))) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_text(aes(label = yval), position = position_dodge(0.9),
            vjust = -0.5) + theme_minimal() + xlab("Status") + ylab("Percentage of sex (%)") +  scale_y_continuous(limits = c(0,1), breaks=seq(0,1,by=0.25), labels=c("0%", "25%", "50%", "75%", "100%")) + scale_fill_discrete(name="Sex")


#Wordcloud
#word cloud
# library(stringr)
# library(tidytext)
# library(dplyr)
# library(wordcloud)
# text <- data.frame(cupid$sex, cupid$essay0)
# text$cupid.essay0 <- str_replace_all(text$cupid.essay0, "\n", " ")
# text$cupid.essay0 <- str_replace_all(text$cupid.essay0, "<br />", " ")
# text$cupid.essay0 <- gsub("<[^>]+>", "",text$cupid.essay0)
# text$cupid.essay0 <- gsub('[[:digit:]]+', '', text$cupid.essay0)
# text <- text %>% unnest_tokens(word,cupid.essay0)
# data("stop_words")
# tidy_text <- text %>% anti_join(stop_words)
# tidy_text %>% dplyr::count(word, sort = TRUE)
# #tidy_text %>% with(wordcloud(word,n,max.words = 100))
# freq.text <- tidy_text %>% group_by(cupid.sex) %>% dplyr::count(word, sort=TRUE) %>% left_join(tidy_text %>% group_by(cupid.sex) %>% summarise(total = n())) %>% mutate(freq=n/total)
# 
# library(tidyr)
# word_ratios <- tidy_text %>% dplyr::count(word, cupid.sex) %>% filter(sum(n)>=10) %>% ungroup() %>% spread(cupid.sex,n,fill=0) %>% mutate_if(is.numeric, funs((. + 1)/sum(. + 1))) %>% mutate(logratio = log(f/m)) %>% arrange(desc(logratio))
# word_ratios %>% arrange(abs(logratio))
# 
# word_ratios %>% group_by(logratio <0) %>% top_n(15, abs(logratio)) %>% ungroup() %>% mutate(word = reorder(word, logratio)) %>% ggplot(aes(word,logratio, fill=logratio<0)) + geom_col() + coord_flip() + ylab("log odds ratio (female/male)") + theme_minimal() + ggtitle("The top 15 most distinctive words for each sex") + scale_fill_discrete(name="Sex", labels=c("f","m"))
# 
# words.wom <- na.omit(tidy_text[which(tidy_text$cupid.sex=="f"),])
# words.man <- na.omit(tidy_text[which(tidy_text$cupid.sex=="m"),])
# 
# freq.wom <- freq.text[which(freq.text$cupid.sex=="f"),]
# freq.man <- freq.text[which(freq.text$cupid.sex=="m"),]
# 
# wordcloud(freq.wom$word, freq.wom$freq, min.freq =3, random.order = FALSE, random.color = FALSE, colors=c("firebrick1","firebrick2","firebrick3","red4"), max.words = 50)
# wordcloud(freq.man$word, freq.man$freq, min.freq =3, random.order = FALSE, random.color = FALSE, colors=c("dodgerblue2","dodgerblue3","dodgerblue4","darkblue"), max.words = 50)
