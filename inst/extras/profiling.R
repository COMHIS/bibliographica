library(profvis)
p <- profvis({polish_physical_extent(c("1to", "1leaf", "2 v. 3 pages", "3v.([2],x,[16],207,[1],207-862,[6]p.),plates"))}); print(p)

