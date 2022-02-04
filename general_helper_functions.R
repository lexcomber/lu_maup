## General helper functions

# creates area proportionate grid 
convert_to_lu_grid = function(pixwidth = 10, data = data) {
  #### do gridding
  dsf = data
  # define a grid
  width_in_pixels <- pixwidth
  # dx is the width of a grid cell in meters
  dx <- round( (st_bbox(dsf)["xmax"] - 
                  st_bbox(dsf)["xmin"]) / width_in_pixels, 0)
  # dy is the height of a grid cell in meters
  dy <- dx
  # calculate the height in pixels of the resulting grid
  height_in_pixels <- round( (st_bbox(dsf)["ymax"] - 
                                st_bbox(dsf)["ymin"]) / dy,0)
  grid <- st_make_grid(dsf, cellsize = dx,
                       n = c(width_in_pixels, height_in_pixels),
                       what = "polygons")
  g = data.frame(gID = 1:length(grid))
  st_geometry(g) = grid
  # do overlay
  ol = st_intersection(g,dsf)
  ol$Area = st_area(ol)
  # process overlay
  lu_list = unique(bhab_crop_lut$new) 
  #own_list = as.character(unique(dsf$holding)[order(unique(dsf$holding))])
  df = st_drop_geometry(ol)
  g_list = sort(unique(df$gID))
  g_vec = vector()
  for(i in g_list) {	
    df.i = df[df$gID == i,]
    lu_area_vec = rep(NA,length(lu_list))
    for(j in 1:length(lu_list)){
      lu.j = lu_list[j]
      df.ij = df.i[df.i$lu_class == lu.j,]
      lu_area_vec[j] = as.vector(sum(df.ij$Area, na.rm = T))
    }
    lu_area_vec = lu_area_vec/sum(lu_area_vec)
    g_vec = rbind(g_vec, c(i, lu_area_vec))
  }
  colnames(g_vec) = c("gID",lu_list)
  g_class = unlist(apply(g_vec[,2:11], 1, function(x) lu_list[which.max(x)]))
  lu_class_p = apply(g_vec[,2:11], 1, max)
  df = data.frame(g_vec, lu_class = g_class, lu_class_p)	
  # link back to grid
  g %>% left_join(df) -> g 
  g %>% left_join(
    (bhab_crop_lut[,c(2,3,4:7)] %>% distinct()),
    by = c("lu_class" = "new")) -> g 
  # do ES surfaces 
  West = function(dx) {seq(1,0.01,len=dx)} 
  East = function(dx)	{seq(0.01,1,len=dx)}
  North = function(dy){seq(0.01,1,len=dy)} 
  South = function(dy){seq(1, 0.01,len=dy)}
  Linear = function(x,y){2*x+3*y} 
  NonLinear = function(X,Y){2./exp((X-.5)^2+Y^2)-2./exp((X+.5)^2+Y^2)}
  g$wtP = rescale(as.vector(outer(East(pixwidth),North(pixwidth),Linear)), c(0.05, 5))
  g$wtR = rescale(as.vector(outer(East(pixwidth),North(pixwidth),NonLinear)), c(0.05, 5))
  g$wtC = rescale(as.vector(outer(West(pixwidth),North(pixwidth),Linear)), c(0.05, 5))
  #tm_shape(g)+tm_fill("wt3")	
  return(g)
}
