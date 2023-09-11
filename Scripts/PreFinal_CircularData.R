#Script to make a measurement of the circular mean of the data and visualize it

EmlenFunnelData<-(Method_1.Manual_counting) #Put the data of the emlen funnels in this place. Use the 24 pieces emlen funnel data
sel_cols_Grades <- EmlenFunnelData %>% select(8:31)  #For select the columns where the scratches for each grade is written. NUMBERS SHOULD MATCH WITH THE COLUMNS IN YOUR EXCEL!!!
sel_cols_Grades.t<-as.table(t(sel_cols_Grades))  #In this I rotate the table to plot it, the original is going to be used to make the calculations
janitor::row_to_names(sel_cols_Grades.t, 1) #Here I put the number of the individual as the name of columns

rose.diag(sel_cols_Grades[2], units = "hours", template="clock24", rotation="counter" )


#QUIERO HACER UNA FUNCION QUE ME HAGA LA MEDIA, EL r Y LA DESVIACION ESTANDAR DE LOS DATOS CIRCULARES

sel_cols_Grades<-circular(sel_cols_Grades, type = , units = "frequencies", template = "geographics", rotation = "counter")
windrose(sel_cols_Grades[3])
plot(sel_cols_Grades)
