# Make sure we have all the packages we need
deps <- c("ggplot2", "reshape2")
new.packages <- deps[!(deps %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
for(dep in deps) do.call("library",list(dep))

conf <- table(read.csv("./nb-confusion.csv"))

g <- ggplot(as.data.frame(conf ), aes(y=true, x=predicted, fill=Freq))+geom_tile() 
ggsave("nb-confusion.pdf", plot=g)