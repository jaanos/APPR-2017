library(htmltab)

moski <- htmltab("http://en.wikipedia.org/wiki/List_of_the_longest_ski_jumps", 1)
zenske <- htmltab("http://en.wikipedia.org/wiki/List_of_the_longest_ski_jumps", 2)

rekordi <- rbind(moski, zenske)
rekordi$Gender <- c(rep("M", 115), rep("Z",31))
rekordi$Feet <- NULL
rekordi$Yards <- NULL
rekordi$No. <- NULL

rekordi$Country[rekordi$Country == "Â Norway"] <- "Norway"
