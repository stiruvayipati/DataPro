#This Script will help you to build a manual key color scale for data in R (for Rows & Columns)
#REFERENCE - https://stackoverflow.com/questions/55187154/ggplot-match-color-code-column-to-another-column-in-r-dataframe
###EXAMPLE FROM INSIDE R - Set your colors and data here - use this to modify what key scale output based on your needs 
#real <- c("ST10","ST11","ST131","ST21","ST17","ST93","ST16","ST95","ST69","ST73","ST38","ST655")
#colorcode <- c("#0000FF","#A52A2A","#7FFF00","#FF7F50","#FF1493","#9932CC","#FFD700","#9630b0","#DDA0DD","#FFFF00","#3E8700","#4682B4")
#rc1key <- data.frame(real,colorcode)
#real <- c("ARGannot","oriT","T4SS","PlasmidFinder","Plasmid18Replicons")
#colorcode <- c("#FFD700","#A52A2A","#9630b0","#4682B4","#7FFF00")
#cc1key <- data.frame(real,colorcode)
###EXAMPLE FOR RUNNING ON A FILE
#real     colorcode (COLUMNS)
#ARGannot FFD700
#oriT A52A2A
#real     colorcode (ROWS)
#ST10 0000FF
#ST11 A52A2A

#SYNTAX: Rscript manualKeyR.R rc1key.txt cc1key.txt
library(ggplot2)
library(dplyr)

args <- commandArgs(TRUE)
	if (length(args) != 2) {
		print("Incorrect number of arguments (must be 2)!")
	}	
rowdata <- args[1]
coldata <- args[2]
rc1key <- read.table(rowdata, quote="", header=TRUE, fill=TRUE, sep="\t", stringsAsFactors=FALSE)
cc1key <- read.table(coldata, quote="", header=TRUE, fill=TRUE, sep="\t", stringsAsFactors=FALSE)
rc1key$colorcode <- gsub(rc1key$colorcode , pattern = "^", replacement = "#") #Use this only with args input
cc1key$colorcode <- gsub(cc1key$colorcode , pattern = "^", replacement = "#") #Use this only with args input


rc1key$Serial <- 1
rc1key <- as_tibble(rc1key)
rc1key$real <- as.character(rc1key$real)
rc1key$colorcode <- as.character(rc1key$colorcode)
colors <- distinct(rc1key,real,colorcode)
rcol <- colors$colorcode
names(rcol) <- colors$real
          #columns edits
          cc1key$Serial <- 1
          cc1key <- as_tibble(cc1key)
          cc1key$real <- as.character(cc1key$real)
          cc1key$colorcode <- as.character(cc1key$colorcode)
          colors <- distinct(cc1key,real,colorcode)
          ccol <- colors$colorcode
          names(ccol) <- colors$real

pdf("manualkeyR.pdf")
ggplot(rc1key, aes(x = Serial, y = real, fill = real)) + 
  geom_tile() + 
  theme(legend.position = "none", 
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(5,.001,1,.001, "mm")) +
        scale_fill_manual(values=rcol)
ggplot(cc1key, aes(x = Serial, y = real, fill = real)) + 
  geom_tile() + 
  theme(legend.position = "none", 
        axis.text.x = element_blank(),
        axis.text.y = element_text(size=20),
        axis.title.x = element_blank(),
        axis.title.y = element_blank(), 
        plot.background = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.background = element_blank(),
        panel.border = element_blank(),
        axis.ticks = element_blank(),
        plot.margin = margin(5,.001,1,.001, "mm")) +
        scale_fill_manual(values=ccol)
dev.off()
