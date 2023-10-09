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
# put the elements in a list



#keep three stacked bar plots separate
theme_hhsc2 <- function(){ 
  font <- "Arial"   #assign font family up front
  
  theme_classic() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      panel.grid.major = element_line("black", size = .2), 
      panel.grid.major.x = element_blank(), 
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.ticks = element_blank(),          #strip axis ticks
      
      #text elements
      plot.title = element_text(             #title
        family = font,            #set font family
        size = 12,                #set font size
        face = 'italic',            #bold typeface
        hjust = 0,                #left align
        vjust = 1),               #raise slightly
      
      plot.subtitle = element_text(          #subtitle
        family = font,            #font family
        size = 10),               #font size
      
      plot.caption = element_text(           #caption
        family = font,
        face = 'italic',            #font family
        size = 10,                 #font size
        hjust = 0,                #left align
        margin(1, 3, 1, 3)
      ),               
      
      axis.title = element_text(             #axis titles
        family = font,            #font family
        size = 12),               #font size
      
      axis.text = element_text(              #axis text
        family = font,            #axis famuly
        size = 10,                 #font size
        vjust = 0.5),                
      axis.text.x = element_text(            #margin for axis text
        margin=margin(5)),
      legend.position = 'bottom',
      legend.key.size = unit(.30, 'cm'),
      legend.text = element_text(size=9),
      legend.title = element_text(size=10)
    )
}

theme_hhsc3 <- function(){ 
    font <- "Arial"   #assign font family up front
    
    theme_classic() %+replace%    #replace elements we want to change
    
    theme(
      
      #grid elements
      panel.grid.major = element_blank(), 
      panel.grid.minor = element_blank(),    #strip minor gridlines
      axis.ticks = element_blank(),          #strip axis ticks
      
      #text elements
      plot.title = element_text(             #title
                   family = font,            #set font family
                   size = 10,                #set font size
                   face = 'italic',            #bold typeface
                   hjust = 0,                #left align
                   vjust = 1),               #raise slightly
      
      plot.subtitle = element_text(          #subtitle
                   family = font,            #font family
                   size = 10,
                   vjust = 5),              
      
      plot.caption = element_text(           #caption
                   family = font,
                   face = 'italic',            #font family
                   size = 8,                 #font size
                   hjust = 0,                #left align
                   margin(1, 3, 1, 3)
                   ),               
      
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
      legend.title = element_text(size=9)
    )
      }