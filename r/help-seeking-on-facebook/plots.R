library(ggplot2)
library(dplyr)
library(scales)

# change default color palette
cbPalette <- c("#D55E00", "#0072B2", "#E69F00" ,"#999999", "#56B4E9", "#009E73", "#F0E442", "#CC79A7")

# work on a copy of the data
draw_chart <- function(df, prefix_column, x_title, new_levels, indep_col_name, question, topics) {
  for (i in 1:length(topics)) {
    col_name <- paste(prefix_column, i, sep="")

    # selecting only two wanted columns and renaming them
    cur_df <- df[, c(indep_col_name, col_name)]
    colnames(cur_df) <- c("indep_col" , "dep_col")
    cur_df <- subset(cur_df, dep_col != "" & indep_col != "")

    levels(cur_df$dep_col) <- new_levels

    # calculate the percentages in regard to the independent variable
    cur_df <- cur_df %>%
      group_by(indep_col, dep_col) %>% # NB: the order of the grouping
      summarise(count=n()) %>%
      mutate(perc=count/sum(count))

    diagram_title <- paste(question, topics[i], "?", sep="")

    # draw the diagram
    ggplot(data=cur_df, aes(x=dep_col, y=perc, fill=indep_col)) + # assign how to represent columns
      geom_bar(width=0.7, position=position_dodge(width=0.75), stat="identity") + # adjust width &
      scale_y_continuous(labels=scales::percent, limits=c(0, 0.5)) + # alter y axis
      labs(y = "Percentage", x=x_title, title=diagram_title, fill=indep_col_name) +
      theme_bw() + # change the general theme
      theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank(), plot.title = element_text(hjust = 0.5)) + # remove vertical lines and center title
      scale_fill_manual(values=cbPalette) # changes colors

    # save the image and create folders if needed
    dir_path <- paste(getwd(), "/graphs/", indep_col_name, sep="")
    dir.create(dir_path, recursive=TRUE, showWarnings=FALSE) # ignore warning if folder already existent
    file_name <- paste(x_title, topics[i], ".png", sep="")
    ggsave(file_name, last_plot(), "png", path=dir_path)
  }
}

data = read.csv("./data.csv", header=TRUE, sep=",")

colnames(data) <- c("time","gender", "age", "f_1", "f_2", "f_3", "f_4", "f_5", "f_6", "f_7", "f_8", "f_9", "e_1", "e_2", "e_3", "e_4", "e_5", "e_6", "e_7", "e_8", "e_9")

topics <- c("Technology", "Entertainment", "Home and Family", "Work and Study", "Places to Visit", "Restaurants", "Current Events", "Shopping", "Ethics and Philosophy")

f_levels <- c("never", "rarely", "sometimes", "often", "very often");
e_levels <- c("","1 (ineffective)", "2", "3", "4", "5 (effective)");

f_question <- "How often do you see people asking for help on Facebook\nregarding "
e_question <- "How effective are help-seeking posts on Facebook\nregarding "

# filter out unspecified gender because there weren't enough responses
data = subset(data, gender == "male" | gender == "female")

draw_chart(data, "f_", "Frequency", f_levels, "gender", f_question, topics)
draw_chart(data, "e_", "Effectivity",  e_levels, "gender", e_question, topics)

# merge age buckets into one because there weren't enough responses
i2 <- levels(data$age) %in% c("35 - 44", "45 - 54", "55 - 64", "65 +")
levels(data$age)[i2] <- "35 +"

draw_chart(data, "f_", "Frequency", f_levels, "age", f_question, topics)
draw_chart(data, "e_", "Effectivity",  e_levels, "age", e_question, topics)
