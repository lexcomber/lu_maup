## Paper title: "An investigation of the impacts of scale on Ecosystem Service evaluations and landscape decisions"
## Alexis Comber^1^ and Paul Harris^2^


library(tidyverse)
library(sf)
library(tmap)
library(GISTools)
library(scales)
library(genalg)
library(RColorBrewer)
library(cowplot)
library(grid)
library(gridExtra) 
library(units)
library(gtable)
library(lemon)
library(quadtree)
library(raster)

####### Load some functions
source("general_helper_functions.R")

## needed for some of the plots
source("gga_helper_functions.R")

## the GGA results are re-compiled
## load the script below if you want to run the gga
# source("permute_gga_lu_new.R")

####### PART 1. Load data and lookup 
load("lu_data.RDdata")

## Table 1
bhab_crop_lut = read.csv("bhab_crop_lut3.csv", stringsAsFactors = F)
bhab_crop_lut %>% dplyr::select("new", "bhab_crop") %>% table(.) -> tmp
tmp = apply(as.matrix(tmp), 1, function(x) colnames(tmp)[which(x == 1)])
names(tmp) -> newclass
tmp %>%
  map_chr(paste, collapse = ", ") -> `bhabs`
tab1 = as.tibble(cbind(bhabs, newclass))
names(tab1) = c("Original Classes", "New label")
knitr::kable(tab1, booktabs = T, caption = "\\label{tab:table1}The look-up table used to reclassify the land cover and crop data.")

### Figure 1
# function for basic mapping
map_lu = function(data = data, class = data$lu_class, lwd.val){
  # define col vector
  tab = table(class, data$cols)
  classes = names(apply(tab,1, which.max))
  cols = colnames(tab)[(apply(tab,1, which.max))]
  names(cols) = classes
  # map
  ggplot(data, aes(fill = class)) + geom_sf(lwd = lwd.val)+
	coord_sf(datum=st_crs(27700))+
	scale_fill_manual(values = cols, name = "Land Use:   ") + 
	theme_bw()+
	theme(axis.title.x=element_blank(),
          #axis.text.x=element_blank(),
          axis.ticks.x=element_blank(), 
          axis.title.y=element_blank(),
          #axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          # panel.background = element_blank(), 
          legend.direction = "horizontal", 
          legend.position = "bottom", 
          #legend.key.width = unit(1.1, "cm"),
          legend.text=element_text(size=10))
}

## Figure 1. The land use data, projected to OSGB 1936 (CRS 27700)
map_lu(data = data, class = data$lu_class, lwd.val = 0.1) 


####### PART 2. Create grid data

### Create LU Grids - also are the ES grid - check their contents 
g1000 = convert_to_lu_grid(pixwidth = 2, data = data) # 1km grid
g500 = convert_to_lu_grid(pixwidth = 4, data = data) # 500m grid
g400 = convert_to_lu_grid(pixwidth = 5, data = data) # 400m grid
g200 = convert_to_lu_grid(pixwidth = 10, data = data)# 200m grid
g100 = convert_to_lu_grid(pixwidth = 20, data = data)# 100m grid 
g67 = convert_to_lu_grid(pixwidth = 30, data = data) # 67m grid
g50 = convert_to_lu_grid(pixwidth = 40, data = data) # 50m grid
g40 = convert_to_lu_grid(pixwidth = 50, data = data) # 40m grid
g25 = convert_to_lu_grid(pixwidth = 80, data = data) # 25m grid


## Figure 2 Examples of the land use data aggregated to different scales
## Grid versions of Figure 1
p0 = map_lu(data =g50, class = g50$lu_class, lwd.val = 0.1)
p0 = p0 +theme(legend.direction = "horizontal")
legend_lu <- get_legend(p0)
l2 = map_lu(data =g50, class = g50$lu_class, lwd.val = 0.1) + 
  theme(legend.position="none", 
        axis.text.x=element_blank(),
        axis.text.y=element_blank()) + ggtitle("50 m")
l3 = map_lu(data =g67, class = g67$lu_class, lwd.val = 0.1) + 
  theme(legend.position="none", 
        axis.text.x=element_blank(),
        axis.text.y=element_blank()) + ggtitle("67 m")
l4 = map_lu(data =g100, class = g100$lu_class, lwd.val = 0.1) + 
  theme(legend.position="none", 
        axis.text.x=element_blank(),
        axis.text.y=element_blank()) + ggtitle("100 m")
l5 = map_lu(data =g200, class = g200$lu_class, lwd.val = 0.1) + 
  theme(legend.position="none", 
        axis.text.x=element_blank(),
        axis.text.y=element_blank()) + ggtitle("200 m")
l6 = map_lu(data =g400, class = g400$lu_class, lwd.val = 0.1) + 
  theme(legend.position="none", 
        axis.text.x=element_blank(),
        axis.text.y=element_blank()) + ggtitle("400 m")
l7 = map_lu(data =g500, class = g500$lu_class, lwd.val = 0.1) + 
  theme(legend.position="none", 
        axis.text.x=element_blank(),
        axis.text.y=element_blank()) + ggtitle("500 m")
#grid.arrange(l2, l3, l4, l5, l6, legend_lu, ncol = 3)
plot_grid(l2, l3, l4, l5, l6,l7, NULL, legend_lu, NULL, ncol = 3, rel_heights = c(1, 1, 0.3))


## Figure 3 Examples of an Ecosystem Srervice gradient aggregated to different scales.
## map ES surfaces function
map_ES_grid = function(grid = es20, vals = es20$wtP, pal = "Reds", lwd.val, tit = "Provisioning"){
	ggplot(grid, aes(fill = vals)) + geom_sf(lwd = lwd.val)+
	coord_sf(datum=st_crs(27700))+
	scale_fill_gradientn(colours = brewer.pal(8, pal), name = tit) + 
	theme_bw()+
	theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(), 
          axis.title.y=element_blank(),
          axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          # panel.background = element_blank(), 
          legend.direction = "horizontal", 
          legend.position = "bottom", 
          #legend.key.width = unit(1.1, "cm"),
          legend.text=element_text(size=10),
          legend.title=element_text(size=20))
}
p0 = map_ES_grid(g100, vals = g100$wtR, pal = "Blues", lwd.val = 0.1, "ES score")
p0 = p0 +theme(legend.direction = "horizontal",  legend.title=element_text(size=12)) 
legend_es <- get_legend(p0)
p2 = map_ES_grid(g50, vals = g50$wtR, pal = "Blues", lwd.val = 0.1) + 
  theme(legend.position="none", 
        axis.text.x=element_blank(),
        axis.text.y=element_blank()) + ggtitle("50 m")
p3 = map_ES_grid(g67, vals = g67$wtR, pal = "Blues", lwd.val = 0.1) + 
  theme(legend.position="none", 
        axis.text.x=element_blank(),
        axis.text.y=element_blank()) + ggtitle("67 m")
p4 = map_ES_grid(g100, vals = g100$wtR, pal = "Blues", lwd.val = 0.1) + 
  theme(legend.position="none", 
        axis.text.x=element_blank(),
        axis.text.y=element_blank()) + ggtitle("100 m")
p5 = map_ES_grid(g200, vals = g200$wtR, pal = "Blues", lwd.val = 0.1) + 
  theme(legend.position="none", 
        axis.text.x=element_blank(),
        axis.text.y=element_blank()) + ggtitle("200 m")
p6 = map_ES_grid(g400, vals = g400$wtR, pal = "Blues", lwd.val = 0.1) + 
  theme(legend.position="none", 
        axis.text.x=element_blank(),
        axis.text.y=element_blank()) + ggtitle("400 m")
p7 = map_ES_grid(g500, vals = g500$wtR, pal = "Blues", lwd.val = 0.1) + 
  theme(legend.position="none", 
        axis.text.x=element_blank(),
        axis.text.y=element_blank()) + ggtitle("500 m")

# grid.arrange(p2, p3, p4, p5, p6, legend_es, ncol = 3)
plot_grid(p2, p3, p4, p5, p6, p7, NULL, legend_es, NULL, ncol = 3, rel_heights = c(1, 1, 0.3))


## Table 2
bhab_crop_lut %>% dplyr::select("new", "ScoreR") %>% distinct() %>% 
  filter(new != "Coniferous") %>%
  arrange(new) -> tmp
names(tmp) = c("Land Use", "ES score")
knitr::kable(tmp, booktabs = T, caption = "\\label{tab:table2}The degree of Ecosystem Service provided by each land use class.")

## #### PART 3. Run GA: declare eval data and fixed
## This code needs to be uncommented to run, but it takes time...
## The result of this function is loaded below
##
## source("permute_gga_lu_new.R")
## source("gga_helper_functions.R")
## 
## ## Function
## do_gga = function(g = v200, es.g, fixed, iterations) {
## 
##   fx = fixed.locs.lu(g)
##   if(fixed & length(fx) > 0) {
##     eval_data = make_eval_data(g, es.g, fx)
##     size = nrow(eval_data)
##   } else {
##     fx = NULL
##     eval_data = make_eval_data(g, es.g, fx)
##     size = nrow(eval_data)
##   }
##   eval_data <<- eval_data
##   itercount <<- 0
##   popSize = round(size * 1.2)
##   gga_result = rbga.lu(
## 		#subsize=nrow(g),
## 		size=size,
## 		fixed = fx,
## 		evalFunc=how.good.wtR,
## 		mutationChance = 0.0002,
## 		elitism = floor(popSize/3), # /3NA
## 		monitorFunc=histor.NULL,
## 		iters=iterations,
## 		popSize=popSize)
##   st = best.subset(gga_result)
## 
##   # evaluate the gga result
##   res = evaluate.gga(st, g, es.g, fx)
##   new_st = res$optimalSet
##   #g.opt = create.opt.sf.layer(g, fx, st)
##   g.opt = create.opt.sf.layer(g, new_st, es.g)
##   return(list(gga_result = gga_result,
##               evaluation = res,
##               optimal_sf = g.opt))
## }
## ## Illustration of the loop
## tmp = do_gga(g = data, es.g = g100, fixed = T, iterations = 300)
## plot(tmp$gga_result)
## map_lu(tmp$optimal_sf, class = tmp$optimal_sf$new_lu, lwd.val = 0.1) +
## 	geom_text(data = tmp$optimal_sf, aes(x = X, y = Y,label = round(es_score,1)), size =4)
## 
## sum(tmp$optimal_sf$es_score)
## 
## ## Results for Section 4.1 in the paper: field data with ES grids
## grid_list = paste0("g", c(1000, 500, 400, 200, 100, 67, 50, 40, 25))
## vector_list = list()
## for (i in 1:length(grid_list) ){
##   g = get(grid_list[i])
##   tmp = do_gga(g = data, es.g = g, fixed = T, iterations = 3000)
##   vector_list[[i]] = tmp$evaluation
## }
## names(vector_list) <- grid_list
## save(vector_list, file = "vector_list.RData")
## 
## ## Results for Section 4.3 in the paper: grids with ES grids
## grid_list = paste0("g", c(1000, 500, 400, 200, 100, 67, 50))
## es_grid_list = paste0("g", c(1000, 500, 400, 200, 100, 67, 50, 40, 25))
## gr_vector_list = list()
## counter = 1
## for (i in 1:length(grid_list) ){
##   g = get(grid_list[i])
##   for (j in 1:length(es_grid_list)) {
##     es.g = get(es_grid_list[j])
##     tmp = do_gga(g = g, es.g = es.g, fixed = T, iterations = 3000)
##     gr_vector_list[[counter]] = tmp$evaluation
##     names(gr_vector_list)[[counter]] = paste0(grid_list[i], "_ES", es_grid_list[j])
##     counter = counter + 1
##   }
## }
## save(gr_vector_list, file = "gr_vector_list.RData")



## Load the vector x ES grid optimied data
# source("gga_helper_functions.R")
load("vector_list.RData")
vl = vector_list

# Create map objects of the allocated results for use in Figure 6
new_st = 1:nrow(data)
forig = create.opt.sf.layer(data, new_st, es.g = g100)
m_orig = map_lu(forig, class = forig$new_lu, lwd.val = 0.1) + 
	# geom_text(data = tmp$optimal_sf, aes(x = X, y = Y,label = round(es_score,1)), size =3) +
  ggtitle("Original") +
  theme(legend.position="none", 
        axis.text.x=element_blank(),
        axis.text.y=element_blank())

new_st = vl$g25$optimalSet
f25 = create.opt.sf.layer(data, new_st, es.g = g25)
m25 = map_lu(f25, class = f25$new_lu, lwd.val = 0.1) + 
	# geom_text(data = tmp$optimal_sf, aes(x = X, y = Y,label = round(es_score,1)), size =3) +
  ggtitle("25 m") +
  theme(legend.position="none", 
        axis.text.x=element_blank(),
        axis.text.y=element_blank())

new_st = vl$g40$optimalSet
f40 = create.opt.sf.layer(data, new_st, es.g = g40)
m40 = map_lu(f40, class = f40$new_lu, lwd.val = 0.1) + 
	# geom_text(data = tmp$optimal_sf, aes(x = X, y = Y,label = round(es_score,1)), size =3) +
  ggtitle("40 m") +
  theme(legend.position="none", 
        axis.text.x=element_blank(),
        axis.text.y=element_blank())

new_st = vl$g50$optimalSet
f50 = create.opt.sf.layer(data, new_st, es.g = g50)
m50 = map_lu(f50, class = f50$new_lu, lwd.val = 0.1) + 
	# geom_text(data = tmp$optimal_sf, aes(x = X, y = Y,label = round(es_score,1)), size =3) +
  ggtitle("50 m") +
  theme(legend.position="none", 
        axis.text.x=element_blank(),
        axis.text.y=element_blank())

new_st = vl$g67$optimalSet
f67 = create.opt.sf.layer(data, new_st, es.g = g67)
m67 = map_lu(f67, class = f67$new_lu, lwd.val = 0.1) + 
	# geom_text(data = tmp$optimal_sf, aes(x = X, y = Y,label = round(es_score,1)), size =3) +
  ggtitle("67 m") +
  theme(legend.position="none", 
        axis.text.x=element_blank(),
        axis.text.y=element_blank())

new_st = vl$g100$optimalSet
f100 = create.opt.sf.layer(data, new_st, es.g = g100)
m100 = map_lu(f100, class = f100$new_lu, lwd.val = 0.1) + 
	# geom_text(data = tmp$optimal_sf, aes(x = X, y = Y,label = round(es_score,1)), size =3) +
  ggtitle("100 m") +
  theme(legend.position="none", 
        axis.text.x=element_blank(),
        axis.text.y=element_blank())

new_st = vl$g200$optimalSet
f200 = create.opt.sf.layer(data, new_st, es.g = g200)
m200 = map_lu(f200, class = f200$new_lu, lwd.val = 0.1) + 
	# geom_text(data = tmp$optimal_sf, aes(x = X, y = Y,label = round(es_score,1)), size =3) +
  ggtitle("200 m") +
  theme(legend.position="none", 
        axis.text.x=element_blank(),
        axis.text.y=element_blank())

new_st = vl$g200$optimalSet
f400 = create.opt.sf.layer(data, new_st, es.g = g400)
m400 = map_lu(f400, class = f400$new_lu, lwd.val = 0.1) + 
	# geom_text(data = tmp$optimal_sf, aes(x = X, y = Y,label = round(es_score,1)), size =3) +
  ggtitle("400 m") +
  theme(legend.position="none", 
        axis.text.x=element_blank(),
        axis.text.y=element_blank())

new_st = vl$g500$optimalSet
f500 = create.opt.sf.layer(data, new_st, es.g = g500)
m500 = map_lu(f400, class = f500$new_lu, lwd.val = 0.1) + 
	# geom_text(data = tmp$optimal_sf, aes(x = X, y = Y,label = round(es_score,1)), size =3) +
  ggtitle("500 m") +
  theme(legend.position="none", 
        axis.text.x=element_blank(),
        axis.text.y=element_blank())

new_st = vl$g1000$optimalSet
f1000 = create.opt.sf.layer(data, new_st, es.g = g1000)
m1000 = map_lu(f1000, class = f1000$new_lu, lwd.val = 0.1) + 
	# geom_text(data = tmp$optimal_sf, aes(x = X, y = Y,label = round(es_score,1)), size =3) +
  ggtitle("1000 m") +
  theme(legend.position="none", 
        axis.text.x=element_blank(),
        axis.text.y=element_blank())


## create a table of Original and Optimised ES scores for use in Figure 4
es_orig = rev(unlist(vl)[grep("originalES", names(unlist(vl)))])
es_opt = rev(unlist(vl)[grep("optimalES", names(unlist(vl)))])
tab = tibble(`Aggregation \nScale (m)` = c(25,40,50,67, 100, 200, 400, 500, 1000),
           `Original` = round(es_orig,0),
           `Optimised` = round(es_opt, 0), row.names = NULL)
#knitr::kable(tab, booktabs = T, caption = "\\label{tab:table3}The ES scores for the analysis of field (vector) data under different scales of ES aggregation.")


## Figure 4 The Original and Optimised Ecosystem Service scores of the vector field data when evaluated over ES gradiatents at different resolutions.
tab %>% pivot_longer(cols = c("Original", "Optimised"), names_to = "ES", values_to = "Score") %>% 
  ggplot(aes(x = `Aggregation \nScale (m)`, y = Score, col = ES)) +
  geom_point() + geom_line() + 
  scale_colour_brewer(palette = "Set1", name = "ES Score") +
  xlab("Agrregation scale (m)") +
  ylab("Overall Ecostystem Service Score") +
  theme_bw() -> gr1
mytheme <- gridExtra::ttheme_minimal(base_size = 9,
  core = list(padding = unit(c(2.5, 2.5), "mm")))
tbl <- tableGrob(tab, theme = mytheme, rows = NULL)
grid.arrange(gr1,tbl,ncol = 2,as.table = TRUE, widths = c(4, 2))


## Create data frame for use in Figure 5
data$area = st_area(data)
totArea = as.vector(sum(data$area))
data %>% st_drop_geometry() %>% mutate() %>%
  group_by(lu_class) %>%
  summarise(cl_area = round(as.vector(sum(area)/totArea),3)) -> tab
tab.orig = tibble(`Land Use` = tab$lu_class,
           `Original \nProportion` = tab$cl_area)
tab.orig %>% 
  filter(`Land Use` != "Water") %>%
  filter(`Land Use` != "Broadleaf") %>%
  filter(`Land Use` != "Urban") -> tab.orig
get_optimsed_lu = function(f = f25) {
  classes = tibble(new_lu = sort(unique(data$lu_class)))
  f$area = st_area(f) 
  totArea = sum(f$area)
  totES = sum(f$es_score)
  f %>% st_drop_geometry() %>%  
    group_by(new_lu) %>% 
    summarise(luArea = sum(area)/totArea,
              esScore = sum(es_score)/totES) -> f
  classes %>% left_join(f)
}
#get_optimised_lu(f = f500)
f_list = paste0("f", c(1000, 500, 400, 200, 100, 67, 50, 40, 25))
res_area = res_es = vector()
counter = 1
for(i in f_list) {
  f= get(i)
  res.i = get_optimsed_lu(f)
  res.i[, 2] = as.vector(unlist(res.i[,2]))
  if (counter == 1) {
    res_mat_area = res.i[, c(1,2)]
    res_mat_es = res.i[, c(1,3)]
  }
  if (counter > 1) {
    res_mat_area = cbind(res_mat_area, res.i[,2])
    res_mat_es = cbind(res_mat_es, res.i[,3])
  }
  counter = counter + 1
}
names(res_mat_area)[-1] = f_list 
names(res_mat_es)[-1] = f_list 

tab = table(data$lu_class, data$cols)
classes = names(apply(tab,1, which.max))
cols = colnames(tab)[(apply(tab,1, which.max))]
names(cols) = classes
cols = cols[-c(2,6,7)]


## Figure 5 The land use (solid line) and ES scores (dashed line) proportions when optimised using an ES gradient aggregated to different resolutions
res_mat_area %>% filter(new_lu != "Water") %>%
  filter(new_lu != "Broadleaf") %>%
  filter(new_lu != "Urban") %>% 
  pivot_longer(cols = starts_with("f"), names_to = "Aggregation", values_to = "Proportion") %>%
  separate(col = "Aggregation", c(NA, "Scale"), sep = "[^[:digit:]]+") %>%
  mutate(Scale = as.numeric(Scale)) %>%
  mutate(type = "LU") -> tmp1
res_mat_es %>% filter(new_lu != "Water") %>%
  filter(new_lu != "Broadleaf") %>%
  filter(new_lu != "Urban") %>% 
  pivot_longer(cols = starts_with("f"), names_to = "Aggregation", values_to = "Proportion") %>%
  separate(col = "Aggregation", c(NA, "Scale"), sep = "[^[:digit:]]+") %>%
  mutate(Scale = as.numeric(Scale), type = "ES") -> tmp2
gr2 = ggplot() +
  geom_point(data = tmp1, aes(x = Scale, y = Proportion, col = new_lu)) + 
  geom_line(data = tmp1, aes(x = Scale, y = Proportion, col = new_lu)) +
  #scale_colour_manual(values = cols, name = "Land Use") +
  geom_point(data = tmp2, aes(x = Scale, y = Proportion, col = new_lu)) + 
  geom_line(data = tmp2, aes(x = Scale, y = Proportion, col = new_lu), lty = 2) +
  scale_colour_manual(values = cols, name = "") +
  xlab("ES Aggregation resolution (m)") +
  ylab("Optimised Proportion") +
  theme_bw()+
  theme(  legend.background = element_rect(fill = "grey95"), 
    legend.direction = "horizontal", 
          legend.position = "bottom", 
          #legend.key.width = unit(1.1, "cm"),
          legend.text=element_text(size=9))
tbl <- tableGrob(tab.orig, theme = mytheme, rows = NULL)
grid.arrange(gr2,tbl,ncol = 2,as.table = TRUE, widths = c(4, 2))


## Figure 6 The land use allocations arising from ES gradients aggregated to different scales
p0 = map_lu(data =g50, class = g50$lu_class, lwd.val = 0.1) +theme(legend.direction = "horizontal")
legend_lu <- get_legend(p0) 
p0 = ggdraw()
#grid.arrange(m25, m40, m50, m67, m100, m200, m400, m500, m1000, p0, legend_lu, p0)
plot_grid(m25, m40, m50, m67, m100, m200, m400, m500, m1000, p0, legend_lu, p0, ncol = 3, rel_heights = c(1, 1, 1, 0.3))


## Figure 7 The Original and Optimised Ecosystem Service scores (y-axis) of the gridded Land Use data, evaluated over different ES gradients (x-axis) aggregated over different scales

load("gr_vector_list.RData")
# names(gr_vector_list)
# unlist(gr_vector_list[1])[1:2]

# extract original and optimal scores
orig_vec = unlist(lapply(gr_vector_list, function(x) unlist(x)[1]))
opt_vec = unlist(lapply(gr_vector_list, function(x) unlist(x)[2]))

# sort out the naming - mistake for g50 - labelled g25
#names(gr_vector_list) = gsub("g25_", "g50_", names(gr_vector_list))
#names(orig_vec) = gsub("g25_", "g50_", names(orig_vec))
#names(opt_vec) = gsub("g25_", "g50_", names(opt_vec))


names(orig_vec) %>% as_tibble() %>%
  separate(col = "value", c("Grid", "ES", NA), sep = "[^[:alnum:]]+") %>%
  separate(col = "Grid", c(NA, "LUg"), sep = "[^[:digit:]]+") %>%
  separate(col = "ES", c(NA, "ESg"), sep = "[^[:digit:]]+") %>%
  mutate(LUg = as.numeric(LUg), ESg = as.numeric(ESg), Type = "Original") %>% 
  cbind(ES = as.vector(orig_vec)) -> orig_tab

names(opt_vec) %>% as_tibble() %>%
  separate(col = "value", c("Grid", "ES", NA), sep = "[^[:alnum:]]+") %>%
  separate(col = "Grid", c(NA, "LUg"), sep = "[^[:digit:]]+") %>%
  separate(col = "ES", c(NA, "ESg"), sep = "[^[:digit:]]+") %>%
  mutate(LUg = as.numeric(LUg), ESg = as.numeric(ESg), Type = "Optimised") %>% 
  cbind(ES = as.vector(opt_vec)) -> opt_tab

g_tab = rbind(orig_tab, opt_tab)
# g_tab$LUg
# head(g_tab)

# basic facet
lu_names = c(`50` = "LU @ 50 m",
             `67` = "LU @ 67 m",
             `100` = "LU @ 100 m",
             `200` = "LU @ 200 m",
             `400` = "LU @ 400 m",
             `500` = "LU @ 500 m",
             `1000` = "LU @ 1000 m",
             `9999` = "LU scale equals ES scale")
g_tab %>% filter(ESg == LUg) %>% mutate(LUg = 9999) %>% 
  rbind(g_tab) %>% 
  ggplot(aes(x = ESg, y = ES, col = Type)) +
  geom_line() + geom_point(size = 0.5) +
  facet_wrap(~LUg, ncol = 4, 
             #strip.position = "bottom", 
             labeller = as_labeller(lu_names)) +
  theme_bw() + 
  theme(  legend.direction = "horizontal", 
          legend.position = "bottom") +
  scale_colour_brewer(palette = "Set1", name = "ES Score") +
  xlab("Agrregation scale (m)") +
  ylab("Overall ES Score") 


## Figure 8 Histograms of the square root of area and boundary length of the original land use parcels
data %>% filter(lu_class != "Water") %>%
  filter(lu_class != "Broadleaf") %>%
  filter(lu_class != "Urban") %>% 
  st_boundary() %>% st_length() -> bound
data %>% filter(lu_class != "Water") %>%
  filter(lu_class != "Broadleaf") %>%
  filter(lu_class != "Urban") -> tmp
tmp = data.frame(area = sqrt(as.vector(tmp$area)), boundary = as.vector(bound), ID = 1:length(bound))

tmp %>% pivot_longer(-ID) %>%
  ggplot(aes(x = value)) +
  geom_histogram(aes(y=..density..),bins = 10, fill="indianred", col="white")+
  geom_density(alpha=.3, fill="#FF6666") + theme_bw() + 
  facet_wrap(~name, scale = "free", 
             labeller = as_labeller(c("area"= "Square Root of Area","boundary" = "Boundary Length"))) + 
  xlab("m") + ylab("Density")

## Make map objects for plots in Figures 9 and 10 - this takes time as the grids get finer!
opt_sets = lapply(gr_vector_list, function(x) unlist(x$optimalSet))
#names(opt_sets)
opt_sets_list = (paste0("^g", c(1000, 500, 400, 200, 100, 67, 50), "_"))

for (i in 1:length(opt_sets_list)) {
  index = grep(opt_sets_list[i], names(opt_sets))
  names_list = vector()
  for (j in index) {
    new_st = opt_sets[j]
    g.i.ref = names(new_st) 
    g.i.ref %>% as_tibble() %>% 
      separate(col = "value", c(NA, "ES", "arse"), sep = "[^[:digit:]]+") -> g.i.ref
    g.i = get(paste0("g", g.i.ref[1,1]))
    es.g = get(paste0("g", g.i.ref[1,2]))
    
    fij = create.opt.sf.layer(g.i, as.vector(unlist(new_st)), es.g = es.g)
    mij = map_lu(fij, class = fij$new_lu, lwd.val = 0.1) 
    mij = mij + ggtitle(paste0(g.i.ref[1,2], " m")) +
      theme(legend.position="none", axis.text.x=element_blank(),axis.text.y=element_blank())
    assign(names(new_st), fij)
    assign(paste0("m",names(new_st)), mij)
    names_list <- append(names_list, paste0("m",names(new_st)))
  }

  p0 = map_lu(data =g.i, class = g.i$lu_class, lwd.val = 0.1)
  p0 = p0 +theme(legend.direction = "horizontal")
  legend_lu2 <- get_legend(p0)

  names_list_out <- append(rev(names_list), "legend_lu2")
  l = mget(names_list_out)
  l1 = length(l)
  l = c(l, NA, NA)
  l = l[c(1:(l1-1), (l1+1), l1, (l1+2))]
  plots.i = do.call(plot_grid, args = c(l, ncol = 3, rel_heights = c(1, 1, 1, 0.2)))
  assign(paste0("plotsg",  g.i.ref[1,1]), plots.i)
  #rm(list = names_list)
  cat(i, "\t")
}


## Figure 9 Optimised land use allocations for the 100 m land use grid (right hand side) and 200 m land use grid (left hand side) under different scales of ES gradient aggregation
plot_grid(plotsg100,plotsg200) 

## Figure 10 Optimised land use allocations for the 500 m grid and ES scores generated by an ES gradient aggregated to different scales
val = round(sum(g500_ESg25$wtR * g500_ESg25$ScoreR *g500_ESg25$area), 0)
pp1 = map_lu(g500_ESg25, class = g500_ESg25$new_lu, lwd.val = 0.1) + 
  theme(legend.position="none", axis.text.x=element_blank(),axis.text.y=element_blank()) +
  geom_sf_text(aes(label = round(g500_ESg25$wtR * g500_ESg25$ScoreR *g500_ESg25$area, 0))) + 
  ggtitle(paste0("25 m, ES score ",val))

arse = round(sum(g500_ESg40$wtR * g500_ESg40$ScoreR *g500_ESg40$area), 0)
pp2 = map_lu(g500_ESg40, class = g500_ESg40$new_lu, lwd.val = 0.1) + 
  theme(legend.position="none", axis.text.x=element_blank(),axis.text.y=element_blank()) +
  geom_sf_text(aes(label = round(g500_ESg40$wtR * g500_ESg40$ScoreR *g500_ESg40$area, 0))) + 
  ggtitle(paste0("40 m, ES score ",arse))

arse = round(sum(g500_ESg50$wtR * g500_ESg50$ScoreR *g500_ESg50$area), 0)
pp3 = map_lu(g500_ESg50, class = g500_ESg50$new_lu, lwd.val = 0.1) + 
  theme(legend.position="none", axis.text.x=element_blank(),axis.text.y=element_blank()) +
  geom_sf_text(aes(label = round(g500_ESg50$wtR * g500_ESg50$ScoreR *g500_ESg50$area, 0))) + 
  ggtitle(paste0("50 m, ES score ",arse))

arse = round(sum(g500_ESg67$wtR * g500_ESg67$ScoreR *g500_ESg67$area), 0)
pp4 = map_lu(g500_ESg67, class = g500_ESg67$new_lu, lwd.val = 0.1) + 
  theme(legend.position="none", axis.text.x=element_blank(),axis.text.y=element_blank()) +
  geom_sf_text(aes(label = round(g500_ESg67$wtR * g500_ESg67$ScoreR *g500_ESg67$area, 0))) + 
  ggtitle(paste0("67 m, ES score ",arse))

arse = round(sum(g500_ESg100$wtR * g500_ESg100$ScoreR *g500_ESg100$area), 0)
pp5 = map_lu(g500_ESg100, class = g500_ESg100$new_lu, lwd.val = 0.1) + 
  theme(legend.position="none", axis.text.x=element_blank(),axis.text.y=element_blank()) +
  geom_sf_text(aes(label = round(g500_ESg100$wtR * g500_ESg100$ScoreR *g500_ESg100$area, 0))) + 
  ggtitle(paste0("100 m, ES score ",arse))

aprse = round(sum(g500_ESg200$wtR * g500_ESg200$ScoreR *g500_ESg200$area), 0)
pp6 = map_lu(g500_ESg200, class = g500_ESg200$new_lu, lwd.val = 0.1) + 
  theme(legend.position="none", axis.text.x=element_blank(),axis.text.y=element_blank()) +
  geom_sf_text(aes(label = round(g500_ESg200$wtR * g500_ESg200$ScoreR *g500_ESg200$area, 0))) + 
  ggtitle(paste0("200 m, ES score ",arse))

arse = round(sum(g500_ESg400$wtR * g500_ESg400$ScoreR *g500_ESg400$area), 0)
pp7 = map_lu(g500_ESg400, class = g500_ESg400$new_lu, lwd.val = 0.1) + 
  theme(legend.position="none", axis.text.x=element_blank(),axis.text.y=element_blank()) +
  geom_sf_text(aes(label = round(g500_ESg400$wtR * g500_ESg400$ScoreR *g500_ESg400$area, 0))) + 
  ggtitle(paste0("400 m, ES score ",arse))

arse = round(sum(g500_ESg500$wtR * g500_ESg500$ScoreR *g500_ESg500$area), 0)
pp8 = map_lu(g500_ESg500, class = g500_ESg500$new_lu, lwd.val = 0.1) + 
  theme(legend.position="none", axis.text.x=element_blank(),axis.text.y=element_blank()) +
  geom_sf_text(aes(label = round(g500_ESg500$wtR * g500_ESg500$ScoreR *g500_ESg500$area, 0))) + 
  ggtitle(paste0("500 m, ES score ",arse))

arse = round(sum(g500_ESg1000$wtR * g500_ESg1000$ScoreR *g500_ESg1000$area), 0)
pp9 = map_lu(g500_ESg1000, class = g500_ESg1000$new_lu, lwd.val = 0.1) + 
  theme(legend.position="none", axis.text.x=element_blank(),axis.text.y=element_blank()) +
  geom_sf_text(aes(label = round(g500_ESg1000$wtR * g500_ESg1000$ScoreR *g500_ESg1000$area, 0))) + 
  ggtitle(paste0("1000 m, ES score ",arse))

pp0 = map_lu(g500_ESg25, class = g500_ESg25$new_lu, lwd.val = 0.1)
pp0 = pp0 +theme(legend.direction = "horizontal")
legend_lu3 <- get_legend(pp0)

plot_grid(pp1,pp2,pp3,pp4,pp5,pp6,pp7,pp8,pp9, NULL, legend_lu3, NULL, ncol = 3, rel_heights = c(1,1,1,  0.2))


## Figure 11 A simple quadtree representation of the parcel data, indicating the spatial variation and distribution of potentially 'appropriate' scales of aggregation (grid cell dimentions
library(quadtree)
library(raster)
# create numeric classes
cl = unique(g25$lu_class)
cl = data.frame(lu_class = cl, cln = 1:8)
# join
g25 %>% left_join(cl) -> gq
# create a raster
r <- raster(nrow = 80 , ncols = 80, ext = extent(gq))
p = st_centroid(gq)
r = rasterize(as(p, "Spatial"), r, field = "cln")
# create the quadtree
qt <- quadtree(r, .15, adj_type = "expand", resample_n_side = 64,
               resample_pad_nas = FALSE)
#plot(qt, col = c("white"), border_lwd = 1, legend = F, axes=F, xlab = "", ylab = "")
df = as_data_frame(qt)
# extract polygons coordinates in list format
lst <- lapply(1:nrow(df), function(x){
  ## create a matrix of coordinates that also 'close' the polygon
  res <- matrix(c(df[x, 'xmin'], df[x, 'ymin'],
           df[x, 'xmin'], df[x, 'ymax'],
           df[x, 'xmax'], df[x, 'ymax'],
           df[x, 'xmax'], df[x, 'ymin'],
           df[x, 'xmin'], df[x, 'ymin'])  ## need to close the polygon
         , ncol =2, byrow = T
  )
  ## create polygon objects
  st_polygon(list(res))
})

# create the spatial polygons
sfdf <- st_sf(value = df[, 'value'], st_sfc(lst))
st_crs(sfdf) = 27700
# calculate area and remove the marginal quad
sfdf$area = unlist(as.vector(sqrt(st_area(sfdf))))
sfdf = filter(sfdf, !is.na(value) )
# set the breaks and labels for the plot
breaks = c(0, sort(unique(sfdf$area)))
labels = as.character(round(breaks, 0)[-1])
sfdf <- mutate(sfdf, area = cut(area, breaks, labels =labels))
# undertake the plot
ggplot(sfdf, aes(fill = (area))) + 
  geom_sf(lwd = 0.2)+
	coord_sf(datum=st_crs(27700)) +
	scale_fill_brewer(palette = "Reds", name = "Aggregation Scale (m): \n       (grid cell size)  ") + 
	theme_bw()+
	theme(axis.title.x=element_blank(),
          #axis.text.x=element_blank(),
          axis.ticks.x=element_blank(), 
          axis.title.y=element_blank(),
          #axis.text.y=element_blank(),
          axis.ticks.y=element_blank(),
          # panel.background = element_blank(), 
          legend.direction = "horizontal", 
          legend.position = "bottom", 
          #legend.key.width = unit(1.1, "cm"),
          legend.text=element_text(size=10))

### END ###
