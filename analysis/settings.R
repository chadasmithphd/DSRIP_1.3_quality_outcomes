library(flextable)

set_name <-  "Chad" #"Emily" 

set_flextable_defaults(
        font.family = 'Arial',
        font.size = 10, 
        font.color = "#303030",
        padding = 2, 
        table.layout = "autofit")
        

theme_hhsc <- function(){ 
    font <- "Arial"   #assign font family up front
    
    theme_classic() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      panel.grid.major = element_blank(),
      
      panel.grid.major.y = element_line(color = "black",
                                          size = 0.2),
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.ticks = element_blank(),          #strip axis ticks
      
      #text elements
      plot.title = element_blank(),
      
      plot.subtitle = element_blank(),               #font size
      
      plot.caption = element_blank(),           #caption
      
      axis.title = element_text(             #axis titles
                   family = font,            #font family
                   size = 9),               #font size
      
      axis.text = element_text(              #axis text
                               family = font,            #axis famuly
                               size = 8,                 #font size
                               vjust = 0.5),                
      axis.text.x = element_text(            #margin for axis text
                    margin=margin(5)),
      legend.position = 'bottom',
      legend.key.size = unit(.30, 'cm'),
      legend.text = element_text(size=7.5),
      legend.title = element_text(size=9),
    )
}


# Colors
# Custom theme

mycolors <- c("white", "#d4d4d4", "#666666")
