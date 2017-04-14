library(ggplot2)
library(dplyr)
library(scales)

# work on a copy of the data
do_graph <- function(df, prefix_column, prefix_title, own_levels, indep_col_name) {
  for (i in 1:9) {
    col_name <- paste(prefix_column, i, sep="")
    col_title <- paste(prefix_title, titles[i], sep=" ")

    # selecting only two wanted columns and renaming them
    cur_df <- df[,c(indep_col_name ,col_name)]
    colnames(cur_df) <- c("indep_col" , "dep_col")
    cur_df <- subset(cur_df, dep_col != "" | indep_col != "")

    levels(cur_df$dep_col) <- own_levels

    # group by
    cur_df <- cur_df %>%
      group_by(indep_col, dep_col) %>%
      summarise(count=n()) %>%
      mutate(perc=count/sum(count))

    ggplot(data=cur_df, aes(x=dep_col, y=perc, fill=indep_col)) +
      geom_bar(width=0.7, position=position_dodge(), stat="identity") +
      scale_y_continuous(labels=scales::percent, limits=c(0, 0.5)) +
      labs(y = "Percentage", x=prefix_title, title=col_title, fill=indep_col_name) +
      theme_bw() +
      theme(panel.grid.minor.x=element_blank(), panel.grid.major.x=element_blank())

    # path <- "p"
    dir_path <- paste(getwd(), "/graphs/", indep_col_name, sep="")
    dir.create(dir_path, recursive=TRUE, showWarnings=FALSE)

    file_name <- paste(as.character(col_title), ".png", sep="")
    ggsave(file_name, last_plot(), "png", path=dir_path)
  }
}

data = read.csv("./data.csv", header=TRUE, sep=",")

colnames(data) <- c("time","gender", "age", "f_1", "f_2", "f_3", "f_4", "f_5", "f_6", "f_7", "f_8", "f_9", "e_1", "e_2", "e_3", "e_4", "e_5", "e_6", "e_7", "e_8", "e_9")

titles <- c("Technology", "Entertainment", "Home and Family", "Work and Study", "Places to Visit", "Restaurants", "Current Events", "Shopping", "Ethics and Philosophy")

f_levels <- c("never", "rarely", "sometimes", "often", "very often");
e_levels <- c("","1 (ineffective)", "2", "3", "4", "5 (effective)");

# filter out unspecified gender because of missing results
data = subset(data, gender == "male" | gender == "female")

do_graph(data, "f_", "Frequency", f_levels, "gender")
do_graph(data, "e_", "Effective",  e_levels, "gender")

i2 <- levels(df$age) %in% c("35 - 44", "45 - 54", "55 - 64", "65 +")
levels(df$age)[i2] <- "35 +"

