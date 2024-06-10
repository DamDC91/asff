clear
reset
set border 3

# Each bar is half the (visual) width of its x-range.
set boxwidth 0.02 absolute
set style fill solid 1.0 noborder
filename = ARG1

#bin_width = 1;
#bin_number(x) = floor(x/bin_width)
#rounded(x) = bin_width * ( bin_number(x) + 0.5 )
#rounded2(x)=floor(x)

stats filename using 1

#var=STATS_stddev * STATS_stddev
#mean=STATS_mean
#max=STATS_max
#a=mean*mean/(var)
#b=a/mean

#f(x)=(x**(a-1)) * ((b**a) * exp(-b*x))/(gamma(a))

set term qt 0 title 'histogram'
plot filename smooth frequency with boxes

#set term qt 1 title 'gamma law'
#plot [0:max] f(x) with lines
