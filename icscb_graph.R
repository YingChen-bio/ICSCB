if (!require('ggplot2')) install.packages('ggplot2'); library('ggplot2')
if (!require('dplyr')) install.packages('dplyr'); library('dplyr')
if (!require('plotly')) install.packages('plotly'); library('plotly')

# Please install orca from https://github.com/plotly/orca
# verify that the orca is executable and is available on your PATH.
# The orca command-line utility is required for this functionality.

# country data 
country_list <- read.csv("country_list.csv")
rank_counrty <- country_list[order(country_list$Sum,decreasing=TRUE),]


# top 9 countries and sum for all others
pie_data <- head(rank_counrty,9)
others_sum <- sum(as.numeric(rank_counrty[-c(1:9),]$Sum),na.rm=TRUE)
pie_data <- pie_data[,c(1,4)]
pie_data <- rbind(data.frame(X="Others",Sum=others_sum),pie_data)
pie_data$prop <- round(pie_data$Sum/ sum(pie_data$Sum) *100,1)
pie_data <-pie_data %>%
  arrange(desc(X)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
##
pie_data.sub <- pie_data[pie_data$X!="Others",]
pie_data.others <- pie_data[pie_data$X=="Others",]
pie_data.sub <- pie_data.sub[order(pie_data.sub$Sum,decreasing = TRUE),]
pie_data <- rbind(pie_data.sub,pie_data.others)
pie_data$X <- factor(pie_data$X,rev(as.character(pie_data$X)))



#country bar for plotly fig2_Country bar
p_country_bar <- plot_ly(pie_data, y=~X, x = ~Sum,
                         type = 'bar', orientation = 'h',
                         marker = list(color = 'rgba(50, 171, 96, 0.6)',
                                       line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) %>%
  layout(margin = list(l = 150, r = 20, b = 10, t = 10),
         yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85),tickfont = list(family = 'Arial',size = 15),title=paste0(c(rep("&nbsp;", 20),"Country",collapse = ""))),
         xaxis = list(title="Cell line count",tickfont = list(family = 'Arial',size = 15),zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE)) %>%
  add_annotations(xref = 'x1', yref = 'y',
                  x = pie_data$Sum *1.1+40,  y = pie_data$X,
                  text = paste(round(pie_data$prop, 2), '%'),
                  font = list(family = 'Arial', size = 15, color = 'rgb(50, 171, 96)'),
                  showarrow = FALSE)
orca(p_country_bar, "country_bars.pdf",width = 1600, height = 1000)

#cell_type_data
celltype_list <- read.csv("stem_cell_type.csv",sep=",")
celltype_list$prop <- round(celltype_list$Counts / sum(celltype_list$Counts) *100,2)
celltype_list <-celltype_list %>%
  arrange(desc(Stem_cell_type)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)
celltype_list$prop <- round(celltype_list$Counts / sum(celltype_list$Counts) *100,1)
# cell type pie
p_cell_type_pie <- plot_ly(celltype_list, labels = ~Stem_cell_type, values = ~Counts, type = 'pie',textposition = 'outside',textinfo = 'label+percent',textfont = list(color = '#000000', size = 25),marker = list(colors=c("#8c564b", "#9467bd", "#d62728",
                                                                                                                                                                                                                            "#11AA22", "#AA231B88"))) %>%
  layout(title = '',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         margin = list(l = 150, r = 150, b = 150, t = 150, pad = 4))
orca(p_cell_type_pie, "cell_types_pie.pdf",width = 1600, height = 1000)

#disease list 
disease_list <- read.csv("disease_stat.csv")
disease_list$prop <- round(disease_list$Counts / sum(disease_list$Counts) *100,1)
disease_list <-disease_list %>%
  arrange(desc(Healthy_status)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)


#disease pie
p_disease_pie <- plot_ly(disease_list, labels = ~Healthy_status, values = ~Counts, type = 'pie',textposition = 'outside',textinfo = 'label+percent',textfont = list(color = '#000000', size = 25)) %>%
  layout(title = 'Healthy-Diseases',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE))

orca(p_disease_pie, "disease_pie.pdf",width = 1600, height = 1000)

# disease_category data

disease_cate <- read.csv("disease_cate.csv")
disease_cate <- aggregate(disease_cate$Counts, by=list(Disease=disease_cate$Disease), FUN=sum)
disease_cate$prop <- round(disease_cate$x / sum(disease_cate$x) *100,1)
disease_cate <-disease_cate %>%
  arrange(desc(Disease)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)

##
disease_cate.sub <- disease_cate[disease_cate$Disease!="Others",]
disease_cate.others <- disease_cate[disease_cate$Disease=="Others",]
disease_cate.sub <- disease_cate.sub[order(disease_cate.sub$x,decreasing = TRUE),]
disease_cate <- rbind(disease_cate.sub,disease_cate.others)
disease_cate$Disease <- factor(disease_cate$Disease,rev(as.character(disease_cate$Disease)))

#disease_bar
p_disease_bar <- plot_ly(disease_cate, y= ~Disease, x = ~x,
                         type = 'bar', orientation = 'h',
                         marker = list(color = 'rgba(255, 204, 127, 0.6)',
                                       line = list(color = 'rgba(255, 163, 25, 1.0)', width = 1))) %>%
  layout(margin = list(l = 150, r = 20, b = 10, t = 10),
         yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = TRUE, tickfont = list(family = 'Arial',size = 15),domain= c(0, 0.85),title=paste0(c(rep("&nbsp;", 20),"Country",collapse = ""))),
         xaxis = list(title="Disease count",tickfont = list(family = 'Arial',size = 15),zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE)) %>%
  add_annotations(xref = 'x1', yref = 'y',
                  x = disease_cate$x *1.1+25,  y = disease_cate$Disease,
                  text = paste(round(disease_cate$prop, 2), '%'),
                  font = list(family = 'Arial', size = 12, color = 'rgb(255, 148, 0)'),
                  showarrow = FALSE)
orca(p_disease_bar, "p_disease_bar.pdf",width = 1600, height = 1000)

# supplementary data 
# all DB cell type
celltype_list <- read.csv("stem_cell_type_sup.csv")
celltype_list$prop <- round(celltype_list$hpscreg / sum(celltype_list$hpscreg) *100,1)
celltype_list <-celltype_list %>%
  arrange(desc(Stem_cell_type)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)

p_sup_cell_type_hpscreg <- plot_ly(celltype_list, labels = ~Stem_cell_type, values = ~hpscreg, type = 'pie',textposition = 'outside',textinfo = 'label+percent',textfont = list(color = '#000000', size = 25),marker = list(colors=c("#8c564b", "#9467bd", "#d62728",
                                                                                                                                                                                                                                     "#11AA22", "#AA231B88"))) %>%
  layout(title = '',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         margin = list(l = 150, r = 150, b = 150, t = 150, pad = 4))

orca(p_sup_cell_type_hpscreg,"p_hpscreg_pie.pdf",width = 1600, height = 1000)

celltype_list$prop <- round(celltype_list$skip / sum(celltype_list$skip) *100,1)
celltype_list <-celltype_list %>%
  arrange(desc(Stem_cell_type)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)

p_sup_cell_type_skip <- plot_ly(celltype_list, labels = ~Stem_cell_type, values = ~skip, type = 'pie',textposition = 'outside',textinfo = 'label+percent',textfont = list(color = '#000000', size = 25),marker = list(colors=c("#8c564b", "#9467bd", "#d62728",
                                                                                                                                                                                                                               "#11AA22", "#AA231B88"))) %>%
  layout(title = '',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         margin = list(l = 150, r = 150, b = 150, t = 150, pad = 4))
orca(p_sup_cell_type_skip,"p_skip_pie.pdf",width = 1600, height = 1000)



celltype_list$prop <- round(celltype_list$riken / sum(celltype_list$riken) *100,1)
celltype_list <-celltype_list %>%
  arrange(desc(Stem_cell_type)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)

p_sup_cell_type_riken<- plot_ly(celltype_list, labels = ~Stem_cell_type, values = ~riken, type = 'pie',textposition = 'outside',textinfo = 'label+percent',textfont = list(color = '#000000', size = 25),marker = list(colors=c("#8c564b", "#9467bd", "#d62728",
                                                                                                                                                                                                                                "#11AA22", "#AA231B88"))) %>%
  layout(title = '',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         margin = list(l = 150, r = 150, b = 150, t = 150, pad = 4))

orca(p_sup_cell_type_riken,"p_riken_pie.pdf",width = 1600, height = 1000)

celltype_list$prop <- round(celltype_list$eagli / sum(celltype_list$eagli) *100,1)
celltype_list <-celltype_list %>%
  arrange(desc(Stem_cell_type)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)

p_sup_cell_type_eagli <- plot_ly(celltype_list, labels = ~Stem_cell_type, values = ~eagli, type = 'pie',textposition = 'outside',textinfo = 'label+percent',textfont = list(color = '#000000', size = 25),marker = list(colors=c("#8c564b", "#9467bd", "#d62728",
                                                                                                                                                                                                                                 "#11AA22", "#AA231B88"))) %>%
  layout(title = '',
         xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
         margin = list(l = 150, r = 150, b = 150, t = 150, pad = 4))

orca(p_sup_cell_type_eagli,"p_eaglei_pie.pdf",width = 1600, height = 1000)

# country data 
country_list <- read.csv("country_list.csv")
rank_counrty <- country_list[order(country_list$SKIP,decreasing=TRUE),]
pie_data <- head(rank_counrty,50)
pie_data$prop <- round(pie_data$SKIP/ sum(pie_data$SKIP) *100,2)
pie_data <-pie_data %>%
  arrange(desc(X)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)


p_sup_country_skip <- plot_ly(pie_data, y= ~reorder(X,SKIP), x = ~SKIP,
                              type = 'bar', orientation = 'h',
                              marker = list(color = 'rgba(50, 171, 96, 0.6)',
                                            line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) %>%
  layout(margin = list(l = 150, r = 20, b = 10, t = 10),
         yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85),title=paste0(c(rep("&nbsp;", 20),"Country",collapse = ""))),
         xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE)) %>%
  add_annotations(xref = 'x1', yref = 'y',
                  x = pie_data$SKIP *1.1+40,  y = pie_data$X,
                  text = round(pie_data$SKIP, 3),
                  font = list(family = 'Arial', size = 12, color = 'rgb(50, 171, 96)'),
                  showarrow = FALSE)

orca(p_sup_country_skip,"p_skipcountry_pie.pdf",width = 1600, height = 1000)

rank_counrty <- country_list[order(country_list$hPSCreg,decreasing=TRUE),]
pie_data <- head(rank_counrty,50)
pie_data$prop <- round(pie_data$hPSCreg/ sum(pie_data$hPSCreg) *100,2)
pie_data <-pie_data %>%
  arrange(desc(X)) %>%
  mutate(lab.ypos = cumsum(prop) - 0.5*prop)


p_sup_country_hpscreg <- plot_ly(pie_data, y= ~reorder(X,hPSCreg), x = ~hPSCreg,
                                 type = 'bar', orientation = 'h',
                                 marker = list(color = 'rgba(50, 171, 96, 0.6)',
                                               line = list(color = 'rgba(50, 171, 96, 1.0)', width = 1))) %>%
  layout(margin = list(l = 150, r = 20, b = 10, t = 10),
         yaxis = list(showgrid = FALSE, showline = FALSE, showticklabels = TRUE, domain= c(0, 0.85),title=paste0(c(rep("&nbsp;", 20),"Country",collapse = ""))),
         xaxis = list(zeroline = FALSE, showline = FALSE, showticklabels = TRUE, showgrid = TRUE)) %>%
  add_annotations(xref = 'x1', yref = 'y',
                  x = pie_data$hPSCreg *1.1+40,  y = pie_data$X,
                  text = round(pie_data$hPSCreg, 3),
                  font = list(family = 'Arial', size = 12, color = 'rgb(50, 171, 96)'),
                  showarrow = FALSE)
orca(p_sup_country_hpscreg,"p_hpscregcountry.pdf",width = 1600, height = 1000)
