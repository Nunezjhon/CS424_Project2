<h2> Authors: Jhon Nunez & Daisy Arellano </h2>

<h3> Every Breath You Take </h3>

<h4> Visualization of air quality data around the US </h4>

<h5> Instructions </h5>

To view the YouTube video: Click <a href="https://www.youtube.com/watch?v=DfB2KeXQ6_s">here</a>  
To view the shinyapp dashboard: Click <a href="http://shiny.evl.uic.edu:3838/g9/CS424_Project2/">here</a> 

Our application allows the user to start by picking a year, county and state they would like to visualize.
These can be chosen by a drop down menu which updates as the user clicks on a state or year to show only the information for which there exists data.
Our app functions for all years of all counties for yearly and daily data thus fulfilling B and C level requirements about data downloading. Also functions for hourly data which we will discuss later. 

![project2](https://user-images.githubusercontent.com/35846525/55626730-eb2c9800-5771-11e9-8053-d140c840c235.png)


<h5> Libraries Used: </h5>
ggplot2, lubridate, DT, grid, gridExtra, leaflet, scales, readr, plotly, tidyr, dplyr,f fst, shinyWidgets
<h5> Data:</h5>
Data imported from the United States Environmental Protection Agency 
