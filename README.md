# Insitu-R-Lake-Visualizer
Manipulate and visualize single or multiple Insitu files


#insitu_core.R
-plots the chosen variable against depth in a single chosen insitu file
-the first plot shows downcast data
-the second plot shows both downcast & upcast data


#insitu_compare.R
-plots the chosen variable against depth in multiple files in the chosen directory
-the plot legend shows the corresponding date of each trendline


#insitu_combine.R
-combine multiple insitu files into a single one
-trims the data; only retains downcast data
-the output file has the date column in year-month-day format


#insitu_heatmap.R
-plot heat map (y= depth, x = date, color = chosen varible) of the combined insitu file
-coming soon!
