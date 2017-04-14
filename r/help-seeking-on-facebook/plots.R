library(ggplot2)
library(dplyr)
library(scales)

do_graph <- function(prefix, prefix_title, own_levels) {
  for (i in 1:9) {
    col_name <- paste(prefix, i, sep="")
    col_title <- paste(prefix_title, titles[i], sep=" ")

    d2 <- d1[,c("age",col_name)]
    colnames(d2) <- c("age", "xx")
    d2 <- subset(d2, xx != "")
    d1 <- subset(d1, gender == "female" | gender == "male")

    i2 <- levels(d2$age) %in% c("35 - 44", "45 - 54", "55 - 64", "65 +")
    levels(d2$age)[i2] <- "35 +"

    d2 <- d2 %>%
      group_by(age, xx) %>%
      summarise(count=n()) %>%
      mutate(perc=count/sum(count))

    levels(d2$xx) <- own_levels

    ggplot(data=d2, aes(x=xx, y = perc, fill=age)) +
      geom_bar(width=0.7, position=position_dodge(), stat="identity") +
      scale_y_continuous(labels = scales::percent, limits=c(0, 0.5)) +
      labs(y = "Percentage", x=prefix_title, title = col_title)

    path <- paste("/Users/filter/code/helpseeking/graphs_age/", col_title, ".png", sep="")
    ggsave(path, last_plot(), "png")
  }
}

d1 = read.csv("/Users/filter/code/helpseeking/data.csv", header=TRUE, sep=",")

colnames(d1) <- c("time","gender", "age", "f_1", "f_2", "f_3", "f_4", "f_5", "f_6", "f_7", "f_8", "f_9", "e_1", "e_2", "e_3", "e_4", "e_5", "e_6", "e_7", "e_8", "e_9")

titles <- c("Technology", "Entertainment", "Home and Family", "Work and Study", "Places to Visit", "Restaurants", "Current Events", "Shopping", "Ethics and Philosophy")

do_graph("f_", "Frequency", c("never", "rarely", "sometimes", "often", "very often"))
do_graph("e_", "Effectivity",  c("","1 (ineffective)", "2", "3", "4", "5 (effective)"))
