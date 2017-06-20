library(ggplot2)
require(data.table)
library("ggthemes")
library(plotly)
library(scales) 
library(stringr)
library(ggrepel)


setwd("G:/EGRA_Tables")

val <- c(0.312,	0.380,	.427, .487,	.712,	.539, .353,	.506,	.474,	.306,	.611,	.476 )
year <- c("2014 Baseline"  , "2015/2016 Midline" , "2017 Endline")
grade <- c(2, 2, 2, 2, 2, 2 ,4, 4, 4, 4, 4, 4 )
lang <- c("Kyrgyz" , "Kyrgyz" , "Kyrgyz"  , "Russian" , "Russian" , "Russian" , "Kyrgyz" , "Kyrgyz" , "Kyrgyz"  , "Russian" , "Russian" , "Russian" )
dt <- data.frame(grade, lang, year, val)
dt


grade2 <- dt[ which(dt$grade==2) , ]
grade2

grade4 <- dt[ which(dt$grade==4) , ]
grade4

# Putting grade 2 and grade 4 data sets into a list 
dfList <- list(grade2, grade4)


setwd("H:/ECA Region Projects/QRP Central Asia-D3452/Technical/Data/2017 KG EGRA/Outreg")
lapply(dfList, function(x) {

  grade = x$grade[1]
  words = grade * 20
  
  b <-  ggplot(x, 
               aes(year, val, group=lang, color= lang)) + 
    geom_line() + 
    geom_point(size = 4) + 
    geom_text_repel(aes(label = scales::percent(round(val, digit=2)), hjust=0, vjust=0 ), 
              show.legend= FALSE , colour="black",size=5) +
    scale_y_continuous(labels = scales::percent , breaks = seq(0.2, 0.8, by = 0.1)) + coord_cartesian(ylim=c(0.2,0.8))  +
    labs( x = "Year" , 
          y = "% of Students", 
          title = paste("Percentage of Grade", grade, "Students", "Meeting Oral Reading Fluency Standard by Year and Language") ,
          subtitle = paste("(Reading", words, "words or above)") , 
          caption = "Source: QRP Early Grade Reading Assesment Data \n (Using Equated Oral Reading Fluency) ") + 
    theme_economist_white(gray_bg=FALSE )   + 
    theme(legend.title=element_blank() , 
          # axis.line.y = element_blank(), 
          # axis.title.y = element_blank(), 
          plot.title = element_text(hjust = 0.5 , size = 12, face = "bold", colour = "black"), 
          plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold", colour = "black"))  
  
  b
  ggsave(filename = paste("Grade", grade, "Benchmark", words,"Words.png" ), plot = b  )

})



b <-  ggplot(grade4, aes(lang, val)) +
      geom_bar(aes(fill = year ), stat = "identity", width=0.7 ,position = "dodge") + 
  geom_text(aes(fill = year  , label = scales::percent(round(val, digit=2))),
                    show.legend= FALSE ,size=5 , position = position_dodge(width=0.7)) +
  scale_y_continuous(labels = scales::percent)
b




+ geom_point(size = 4) + geom_text_repel(aes(label = scales::percent(round(val, digit=2))),
                                                           show.legend= FALSE ,size=5) +
      scale_y_continuous(labels = scales::percent) + coord_cartesian(ylim=c(0.1,0.8))  +
      labs( x = "Year" , y = "Percentage of Students Meeting Oral Reading Fluency Standard" ,
            title = "Percentage of Students in Grade 2 Meeting Oral Reading Fluency Standard  by Year and Language",
            subtitle = "(Reading 40 words or above)" ,
            caption = "Source: QRP Early Grade Reading Assesment Data \n (Using Equated Oral Reading Fluency) ") +
      theme_economist_white(gray_bg=FALSE )   +
      theme(legend.title=element_blank() ,
            # axis.line.x = element_blank(), 
            # axis.title.y = element_blank(),
            plot.title = element_text(hjust = 0.5 , size = 12, face = "bold", colour = "black"),
            plot.subtitle = element_text(hjust = 0.5, size = 12, face = "bold", colour = "black"))

b

# 
# 
# # ggsave(filename = "Grade2_KR_benchmark_analysis.png" , plot = b)
# 
# 
# ggplotly(b)
# 
# 
# # 
# 
# library("maps")
# library("ggplot2")
# us <- fortify(map_data('state'), region = 'region')
# gg <-
#   (ggplot()
#    + geom_map(data  =  us, map = us,
#               aes(x = long, y = lat, map_id = region, group = group),
#               fill = 'white', color = 'black', size = 0.25)
#    + coord_map('albers', lat0 = 39, lat1 = 45)
#    + theme_map()
#   )
# gg






