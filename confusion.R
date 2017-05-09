args <- commandArgs(TRUE)
arg <- args[1]
# Make sure we have all the packages we need
deps <- c("ggplot2", "reshape2")
new.packages <- deps[!(deps %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
for(dep in deps) do.call("library",list(dep))

conf <- table(read.csv(paste(arg, "csv", sep=".")))

g <- ggplot(as.data.frame(conf ), aes(y=true, x=predicted, fill=Freq))+geom_tile() 
ggsave(paste(arg, "png", sep="."), plot=g, width=4, height=3, dpi=300)
