#### OLD FIG 2 - REDUCED multi-panel figure for bivariate PDPs - each isotope separate ####
# pred.order <- c("Precip8110Cat", "RunoffCat", "Hg0DryDep", "LOI_PERCENT")
# 
# defined.pred.labs <- c("Mean precip. (mm)",
#   "Mean runoff (mm)",
#   # "Hg0 dry deposition (ng m^-3)",
#   expression(paste("Hg"^"0", " dry dep. (ng ", "m"^"-3", ")")),
#   "Sed. carbon (fraction)")
# 
# pred.labs.line1 <- c("Mean precipitation",
#                      "Mean runoff",
#                      expression(paste("Hg"^"0", " dry deposition")),
#                      "Sediment carbon")
# pred.labs.line2 <- c("(mm)",
#                      "(mm)",
#                      expression(paste("(ng ", "m"^"-3", ")")),
#                      "(fraction)")
# 
# # bquote(atop("first line",
# #             "second line" ~ x ^ 2))
# 
# 
# 
# 
# 
# # D199 
# biplots199 <- vector('list', length(pred.order)*length(pred.order))
# 
# for(j in 1:length(pred.order)){
#   print(j)
#   
#   for(w in 1:length(pred.order)){
#     
#     print(w)
#     
#     biplots199[[(j-1)*(length(pred.order)) + w]] <- local({
#       
#       j <- j
#       w <- w
#       pred.var <- pred.order[j] 
#       
#       pred.var.lab <- pred.var
#       if(pred.var == "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s") pred.var.lab <- "WetLossConv"
#       pred.var.lab.def <- defined.pred.labs[j]
#       
#       pred.var.lab.mid1 <- pred.labs.line1[j]
#       pred.var.lab.mid2 <- pred.labs.line2[j]
#       
#       
#       pred.var.w <- pred.order[w]
#       pred.var.lab2 <- pred.var.w
#       if(pred.var.w == "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s") pred.var.lab2 <- "WetLossConv"
#       
#       pred.var.lab2.def <- defined.pred.labs[w]
#       
#       
#       
#       if(pred.var==pred.var.w){
#         # if(w==1) p199 <- ggplot() + theme_void() + ggtitle(paste(pred.var.lab))
#         # if(w>1 & w<length(pred.order)) p199 <- ggplot() + theme_void() 
#         # if(w==length(pred.order)) p199 <- ggplot() + theme_void() + xlab(paste(pred.var.lab))
#         p199 <- ggplot() + 
#           ylim(0,1) +
#           # annotate("text", x = 4, y = 25, size=16, label = pred.var.lab.mid1) + 
#           annotate("text", x = 4, y = .6, size=22, label = pred.var.lab.mid1) + 
#           annotate("text", x = 4, y = .4, size=22, label = pred.var.lab.mid2) +
#           theme_void()
#         
#       } else{
#         
#         # Read in PDP dat
#         res <- readRDS(paste0(output_dir, "PDP/Bivariate/", paste0(pred.var.lab), "/",paste0(pred.var.lab), "_", paste0(pred.var.lab2),   "_PDP_dat.rds"))
#         
#         # Predicted isos in original units
#         res_origunit <- res
#         
#         # Transform appropriate columns
#         res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D199"), function(x) ( x * Iso_stats$D199[2] ) + Iso_stats$D199[1]))
#         res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D200"), function(x) ( x * Iso_stats$D200[2] ) + Iso_stats$D200[1]))
#         res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D202"), function(x) ( x * Iso_stats$D202[2] ) + Iso_stats$D202[1]))
#         
#         colnames(res_origunit) <- gsub("SD", "origUnits", colnames(res_origunit))
#         
#         
#         
#         # For plotting LOI, multiply LOI by 100 because was unnecessarily divided by 100 an extra time in isotope models. 
#         if(pred.var == "LOI_PERCENT"| pred.var.w == "LOI_PERCENT") res$LOI_PERCENT <- 100*res$LOI_PERCENT
#         if(pred.var == "LOI_PERCENT"| pred.var.w == "LOI_PERCENT") res_origunit$LOI_PERCENT <- 100*res_origunit$LOI_PERCENT
#         
#         # Observed values for 2 vars of interest
#         obs_dat <- pdp_dat %>% dplyr::select(one_of(pred.var), one_of(pred.var.w))  # subset by cluster here in cluster loop
#         if(pred.var == "LOI_PERCENT" | pred.var.w == "LOI_PERCENT") obs_dat$LOI_PERCENT <- 100*obs_dat$LOI_PERCENT
#         
#         
#         
#         # D199 all lakes PDP - origUnit
#         # Center D199 at 0 - others will be centered at the mean
#         # Also use coord_cartesian here because cuts off outer row otherwise
#         
#         if(j==1 & w<length(pred.order)){
#           # y-axis label
#           p199 <- res_origunit %>%
#             ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D199_origUnits)) +
#             theme_minimal() +
#             coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
#             geom_tile(aes(fill=Pred_D199_origUnits)) +
#             geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.25, shape=16, size=1) +
#             geom_contour(color = "white") +
#             scale_fill_continuous_diverging(name="D199", palette = 'Blue-Red', mid=0, alpha=1, rev=F, limits=range199,  p1=.9, l2 = 95) +
#             theme(text=element_text(size=48),
#                   legend.text = element_text(size=48),
#                   legend.title = element_text(size=56),
#                   legend.key.size = unit(2, "cm"),
#                   axis.title.x = element_blank()) +
#             xlab(pred.var.lab.def) +
#             ylab(pred.var.lab2.def)
#         }
#         
#         if(j==1 & ((j-1)*(length(pred.order)) + w) %% length(pred.order) == 0){
#           # both labels
#           p199 <- res_origunit %>%
#             ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D199_origUnits)) +
#             theme_minimal() +
#             coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
#             geom_tile(aes(fill=Pred_D199_origUnits)) +
#             geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.25, shape=16, size=1) +
#             geom_contour(color = "white") +
#             scale_fill_continuous_diverging(name="D199", palette = 'Blue-Red', mid=0, alpha=1, rev=F, limits=range199,  p1=.9, l2 = 95) +
#             theme(text=element_text(size=48),
#                   legend.text = element_text(size=48),
#                   legend.title = element_text(size=56),
#                   legend.key.size = unit(2, "cm")) +
#             xlab(pred.var.lab.def) +
#             ylab(pred.var.lab2.def)
#         }
#         
#         if( (j!=1) & ((j-1)*(length(pred.order)) + w) %% length(pred.order) == 0){
#           # x-axis label
#           p199 <- res_origunit %>%
#             ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D199_origUnits)) +
#             theme_minimal() +
#             coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
#             geom_tile(aes(fill=Pred_D199_origUnits)) +
#             geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.25, shape=16, size=1) +
#             geom_contour(color = "white") +
#             scale_fill_continuous_diverging(name="D199", palette = 'Blue-Red', mid=0, alpha=1, rev=F, limits=range199,  p1=.9, l2 = 95) +
#             theme(text=element_text(size=48),
#                   legend.text = element_text(size=48),
#                   legend.title = element_text(size=56),
#                   legend.key.size = unit(2, "cm"),
#                   axis.title.y = element_blank()) +
#             xlab(pred.var.lab.def) +
#             ylab(pred.var.lab2.def)
#         }
#         
#         if( (j!=1) & (((j-1)*(length(pred.order)) + w) %% length(pred.order) != 0)){
#           # no axis labels
#           p199 <- res_origunit %>%
#             ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D199_origUnits)) +
#             theme_minimal() +
#             coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
#             geom_tile(aes(fill=Pred_D199_origUnits)) +
#             geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.25, shape=16, size=1) +
#             geom_contour(color = "white") +
#             scale_fill_continuous_diverging(name="D199", palette = 'Blue-Red', mid=0, alpha=1, rev=F, limits=range199,  p1=.9, l2 = 95) +
#             theme(text=element_text(size=48),
#                   legend.text = element_text(size=48),
#                   legend.title = element_text(size=56),
#                   legend.key.size = unit(2, "cm"),
#                   axis.title.y = element_blank(),
#                   axis.title.x = element_blank()) +
#             xlab(pred.var.lab.def) +
#             ylab(pred.var.lab2.def)
#         }
#        
#         p199
#       }
#     })
#   }
# }
# 
# # biplots199[[2]]
# 
# wrap.plot199 <- wrap_plots(biplots199, nrow=length(pred.order), ncol=length(pred.order), byrow=FALSE) +
#   plot_layout(guides = 'collect') &
#   theme(legend.position = "right")
# # ggsave(plot=wrap.plot199, filename=paste0(fig_dir, "PDP_SKATER20/D199_REDUCED_Bivariate_PDP_wrapLegendRight_reduceLabs.png"), width=70, height=45, limitsize=FALSE)
# ggsave(plot=wrap.plot199, filename=paste0(fig_dir, "PDP_SKATER20/D199_REDUCED_Bivariate_PDP_wrapLegendRight_reduceLabs.png"), width=40, height=32, limitsize=FALSE)
# 
# 
# 
# 
# 
# 
# 
# 
# # D200 
# biplots200 <- vector('list', length(pred.order)*length(pred.order))
# 
# for(j in 1:length(pred.order)){
#   print(j)
#   
#   for(w in 1:length(pred.order)){
#     
#     print(w)
#     
#     biplots200[[(j-1)*(length(pred.order)) + w]] <- local({
#       
#       j <- j
#       w <- w
#       pred.var <- pred.order[j] 
#       
#       pred.var.lab <- pred.var
#       if(pred.var == "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s") pred.var.lab <- "WetLossConv"
#       pred.var.lab.def <- defined.pred.labs[j]
#       
#       pred.var.lab.mid1 <- pred.labs.line1[j]
#       pred.var.lab.mid2 <- pred.labs.line2[j]
#       
#       
#       pred.var.w <- pred.order[w]
#       pred.var.lab2 <- pred.var.w
#       if(pred.var.w == "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s") pred.var.lab2 <- "WetLossConv"
#       
#       pred.var.lab2.def <- defined.pred.labs[w]
#       
#       
#       
#       if(pred.var==pred.var.w){
#         # if(w==1) p200 <- ggplot() + theme_void() + ggtitle(paste(pred.var.lab))
#         # if(w>1 & w<length(pred.order)) p200 <- ggplot() + theme_void() 
#         # if(w==length(pred.order)) p200 <- ggplot() + theme_void() + xlab(paste(pred.var.lab))
#         p200 <- ggplot() + 
#           ylim(0,1) +
#           # annotate("text", x = 4, y = 25, size=16, label = pred.var.lab.mid1) + 
#           annotate("text", x = 4, y = .6, size=22, label = pred.var.lab.mid1) + 
#           annotate("text", x = 4, y = .4, size=22, label = pred.var.lab.mid2) +
#           theme_void()
#         
#       } else{
#         
#         # Read in PDP dat
#         res <- readRDS(paste0(output_dir, "PDP/Bivariate/", paste0(pred.var.lab), "/",paste0(pred.var.lab), "_", paste0(pred.var.lab2),   "_PDP_dat.rds"))
#         
#         # Predicted isos in original units
#         res_origunit <- res
#         
#         # Transform appropriate columns
#         res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D199"), function(x) ( x * Iso_stats$D199[2] ) + Iso_stats$D199[1]))
#         res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D200"), function(x) ( x * Iso_stats$D200[2] ) + Iso_stats$D200[1]))
#         res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D202"), function(x) ( x * Iso_stats$D202[2] ) + Iso_stats$D202[1]))
#         
#         colnames(res_origunit) <- gsub("SD", "origUnits", colnames(res_origunit))
#         
#         
#         
#         # For plotting LOI, multiply LOI by 100 because was unnecessarily divided by 100 an extra time in isotope models. 
#         if(pred.var == "LOI_PERCENT"| pred.var.w == "LOI_PERCENT") res$LOI_PERCENT <- 100*res$LOI_PERCENT
#         if(pred.var == "LOI_PERCENT"| pred.var.w == "LOI_PERCENT") res_origunit$LOI_PERCENT <- 100*res_origunit$LOI_PERCENT
#         
#         # Observed values for 2 vars of interest
#         obs_dat <- pdp_dat %>% dplyr::select(one_of(pred.var), one_of(pred.var.w))  # subset by cluster here in cluster loop
#         if(pred.var == "LOI_PERCENT" | pred.var.w == "LOI_PERCENT") obs_dat$LOI_PERCENT <- 100*obs_dat$LOI_PERCENT
#         
#         
#         
#         # D200 all lakes PDP - origUnit
#         # Center D200 at 0 - others will be centered at the mean
#         # Also use coord_cartesian here because cuts off outer row otherwise
#         
#         if(j==1 & w<length(pred.order)){
#           # y-axis label
#           p200 <- res_origunit %>%
#             ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D200_origUnits)) +
#             theme_minimal() +
#             coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
#             geom_tile(aes(fill=Pred_D200_origUnits)) +
#             geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.25, shape=16, size=1) +
#             geom_contour(color = "white") +
#             scale_fill_continuous_diverging(name="D200", palette = 'Blue-Red', mid=mean200, alpha=1, rev=F, limits=range200,  p1=.9, l2 = 95) +
#             theme(text=element_text(size=48),
#                   legend.text = element_text(size=48),
#                   legend.title = element_text(size=56),
#                   legend.key.size = unit(2, "cm"),
#                   axis.title.x = element_blank()) +
#             xlab(pred.var.lab.def) +
#             ylab(pred.var.lab2.def)
#         }
#         
#         if(j==1 & ((j-1)*(length(pred.order)) + w) %% length(pred.order) == 0){
#           # both labels
#           p200 <- res_origunit %>%
#             ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D200_origUnits)) +
#             theme_minimal() +
#             coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
#             geom_tile(aes(fill=Pred_D200_origUnits)) +
#             geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.25, shape=16, size=1) +
#             geom_contour(color = "white") +
#             scale_fill_continuous_diverging(name="D200", palette = 'Blue-Red', mid=mean200, alpha=1, rev=F, limits=range200,  p1=.9, l2 = 95) +
#             theme(text=element_text(size=48),
#                   legend.text = element_text(size=48),
#                   legend.title = element_text(size=56),
#                   legend.key.size = unit(2, "cm")) +
#             xlab(pred.var.lab.def) +
#             ylab(pred.var.lab2.def)
#         }
#         
#         if( (j!=1) & ((j-1)*(length(pred.order)) + w) %% length(pred.order) == 0){
#           # x-axis label
#           p200 <- res_origunit %>%
#             ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D200_origUnits)) +
#             theme_minimal() +
#             coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
#             geom_tile(aes(fill=Pred_D200_origUnits)) +
#             geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.25, shape=16, size=1) +
#             geom_contour(color = "white") +
#             scale_fill_continuous_diverging(name="D200", palette = 'Blue-Red', mid=mean200, alpha=1, rev=F, limits=range200,  p1=.9, l2 = 95) +
#             theme(text=element_text(size=48),
#                   legend.text = element_text(size=48),
#                   legend.title = element_text(size=56),
#                   legend.key.size = unit(2, "cm"),
#                   axis.title.y = element_blank()) +
#             xlab(pred.var.lab.def) +
#             ylab(pred.var.lab2.def)
#         }
#         
#         if( (j!=1) & (((j-1)*(length(pred.order)) + w) %% length(pred.order) != 0)){
#           # no axis labels
#           p200 <- res_origunit %>%
#             ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D200_origUnits)) +
#             theme_minimal() +
#             coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
#             geom_tile(aes(fill=Pred_D200_origUnits)) +
#             geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.25, shape=16, size=1) +
#             geom_contour(color = "white") +
#             scale_fill_continuous_diverging(name="D200", palette = 'Blue-Red', mid=mean200, alpha=1, rev=F, limits=range200,  p1=.9, l2 = 95) +
#             theme(text=element_text(size=48),
#                   legend.text = element_text(size=48),
#                   legend.title = element_text(size=56),
#                   legend.key.size = unit(2, "cm"),
#                   axis.title.y = element_blank(),
#                   axis.title.x = element_blank()) +
#             xlab(pred.var.lab.def) +
#             ylab(pred.var.lab2.def)
#         }
#         
#         p200
#       }
#     })
#   }
# }
# 
# # biplots200[[2]]
# 
# wrap.plot200 <- wrap_plots(biplots200, nrow=length(pred.order), ncol=length(pred.order), byrow=FALSE) +
#   plot_layout(guides = 'collect') &
#   theme(legend.position = "right")
# # ggsave(plot=wrap.plot200, filename=paste0(fig_dir, "PDP_SKATER20/D200_REDUCED_Bivariate_PDP_wrapLegendRight_reduceLabs.png"), width=70, height=45, limitsize=FALSE)
# ggsave(plot=wrap.plot200, filename=paste0(fig_dir, "PDP_SKATER20/D200_REDUCED_Bivariate_PDP_wrapLegendRight_reduceLabs.png"), width=40, height=32, limitsize=FALSE)
# 
# 
# 
# 
# 
# # D202 
# biplots202 <- vector('list', length(pred.order)*length(pred.order))
# 
# for(j in 1:length(pred.order)){
#   print(j)
#   
#   for(w in 1:length(pred.order)){
#     
#     print(w)
#     
#     biplots202[[(j-1)*(length(pred.order)) + w]] <- local({
#       
#       j <- j
#       w <- w
#       pred.var <- pred.order[j] 
#       
#       pred.var.lab <- pred.var
#       if(pred.var == "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s") pred.var.lab <- "WetLossConv"
#       pred.var.lab.def <- defined.pred.labs[j]
#       
#       pred.var.lab.mid1 <- pred.labs.line1[j]
#       pred.var.lab.mid2 <- pred.labs.line2[j]
#       
#       
#       pred.var.w <- pred.order[w]
#       pred.var.lab2 <- pred.var.w
#       if(pred.var.w == "WetLossConv_Loss_of_soluble_species_scavenged_by_cloud_updrafts_in_moist_convection_kg_s") pred.var.lab2 <- "WetLossConv"
#       
#       pred.var.lab2.def <- defined.pred.labs[w]
#       
#       
#       
#       if(pred.var==pred.var.w){
#         # if(w==1) p202 <- ggplot() + theme_void() + ggtitle(paste(pred.var.lab))
#         # if(w>1 & w<length(pred.order)) p202 <- ggplot() + theme_void() 
#         # if(w==length(pred.order)) p202 <- ggplot() + theme_void() + xlab(paste(pred.var.lab))
#         p202 <- ggplot() + 
#           ylim(0,1) +
#           # annotate("text", x = 4, y = 25, size=16, label = pred.var.lab.mid1) + 
#           annotate("text", x = 4, y = .6, size=22, label = pred.var.lab.mid1) + 
#           annotate("text", x = 4, y = .4, size=22, label = pred.var.lab.mid2) +
#           theme_void()
#         
#       } else{
#         
#         # Read in PDP dat
#         res <- readRDS(paste0(output_dir, "PDP/Bivariate/", paste0(pred.var.lab), "/",paste0(pred.var.lab), "_", paste0(pred.var.lab2),   "_PDP_dat.rds"))
#         
#         # Predicted isos in original units
#         res_origunit <- res
#         
#         # Transform appropriate columns
#         res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D199"), function(x) ( x * Iso_stats$D199[2] ) + Iso_stats$D199[1]))
#         res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D200"), function(x) ( x * Iso_stats$D200[2] ) + Iso_stats$D200[1]))
#         res_origunit <- res_origunit %>%  mutate(across(starts_with("Pred_D202"), function(x) ( x * Iso_stats$D202[2] ) + Iso_stats$D202[1]))
#         
#         colnames(res_origunit) <- gsub("SD", "origUnits", colnames(res_origunit))
#         
#         
#         
#         # For plotting LOI, multiply LOI by 100 because was unnecessarily divided by 100 an extra time in isotope models. 
#         if(pred.var == "LOI_PERCENT"| pred.var.w == "LOI_PERCENT") res$LOI_PERCENT <- 100*res$LOI_PERCENT
#         if(pred.var == "LOI_PERCENT"| pred.var.w == "LOI_PERCENT") res_origunit$LOI_PERCENT <- 100*res_origunit$LOI_PERCENT
#         
#         # Observed values for 2 vars of interest
#         obs_dat <- pdp_dat %>% dplyr::select(one_of(pred.var), one_of(pred.var.w))  # subset by cluster here in cluster loop
#         if(pred.var == "LOI_PERCENT" | pred.var.w == "LOI_PERCENT") obs_dat$LOI_PERCENT <- 100*obs_dat$LOI_PERCENT
#         
#         
#         
#         # D202 all lakes PDP - origUnit
#         # Center D202 at 0 - others will be centered at the mean
#         # Also use coord_cartesian here because cuts off outer row otherwise
#         
#         if(j==1 & w<length(pred.order)){
#           # y-axis label
#           p202 <- res_origunit %>%
#             ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D202_origUnits)) +
#             theme_minimal() +
#             coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
#             geom_tile(aes(fill=Pred_D202_origUnits)) +
#             geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.25, shape=16, size=1) +
#             geom_contour(color = "white") +
#             scale_fill_continuous_diverging(name="d202", palette = 'Blue-Red', mid=mean202, alpha=1, rev=F, limits=range202,  p1=.9, l2 = 95) +
#             theme(text=element_text(size=48),
#                   legend.text = element_text(size=48),
#                   legend.title = element_text(size=56),
#                   legend.key.size = unit(2, "cm"),
#                   axis.title.x = element_blank()) +
#             xlab(pred.var.lab.def) +
#             ylab(pred.var.lab2.def)
#         }
#         
#         if(j==1 & ((j-1)*(length(pred.order)) + w) %% length(pred.order) == 0){
#           # both labels
#           p202 <- res_origunit %>%
#             ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D202_origUnits)) +
#             theme_minimal() +
#             coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
#             geom_tile(aes(fill=Pred_D202_origUnits)) +
#             geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.25, shape=16, size=1) +
#             geom_contour(color = "white") +
#             scale_fill_continuous_diverging(name="d202", palette = 'Blue-Red', mid=mean202, alpha=1, rev=F, limits=range202,  p1=.9, l2 = 95) +
#             theme(text=element_text(size=48),
#                   legend.text = element_text(size=48),
#                   legend.title = element_text(size=56),
#                   legend.key.size = unit(2, "cm")) +
#             xlab(pred.var.lab.def) +
#             ylab(pred.var.lab2.def)
#         }
#         
#         if( (j!=1) & ((j-1)*(length(pred.order)) + w) %% length(pred.order) == 0){
#           # x-axis label
#           p202 <- res_origunit %>%
#             ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D202_origUnits)) +
#             theme_minimal() +
#             coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
#             geom_tile(aes(fill=Pred_D202_origUnits)) +
#             geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.25, shape=16, size=1) +
#             geom_contour(color = "white") +
#             scale_fill_continuous_diverging(name="d202", palette = 'Blue-Red', mid=mean202, alpha=1, rev=F, limits=range202,  p1=.9, l2 = 95) +
#             theme(text=element_text(size=48),
#                   legend.text = element_text(size=48),
#                   legend.title = element_text(size=56),
#                   legend.key.size = unit(2, "cm"),
#                   axis.title.y = element_blank()) +
#             xlab(pred.var.lab.def) +
#             ylab(pred.var.lab2.def)
#         }
#         
#         if( (j!=1) & (((j-1)*(length(pred.order)) + w) %% length(pred.order) != 0)){
#           # no axis labels
#           p202 <- res_origunit %>%
#             ggplot(aes(x =  get(pred.var), y = get(pred.var.w),  z=Pred_D202_origUnits)) +
#             theme_minimal() +
#             coord_cartesian(xlim = range(res_origunit[pred.var]), ylim=range(res_origunit[pred.var.w]), expand=T) +
#             geom_tile(aes(fill=Pred_D202_origUnits)) +
#             geom_point(data=obs_dat, aes(x=get(pred.var), y=get(pred.var.w)), inherit.aes = F, alpha=.25, shape=16, size=1) +
#             geom_contour(color = "white") +
#             scale_fill_continuous_diverging(name="d202", palette = 'Blue-Red', mid=mean202, alpha=1, rev=F, limits=range202,  p1=.9, l2 = 95) +
#             theme(text=element_text(size=48),
#                   legend.text = element_text(size=48),
#                   legend.title = element_text(size=56),
#                   legend.key.size = unit(2, "cm"),
#                   axis.title.y = element_blank(),
#                   axis.title.x = element_blank()) +
#             xlab(pred.var.lab.def) +
#             ylab(pred.var.lab2.def)
#         }
#         
#         p202
#       }
#     })
#   }
# }
# 
# # biplots202[[2]]
# 
# wrap.plot202 <- wrap_plots(biplots202, nrow=length(pred.order), ncol=length(pred.order), byrow=FALSE) +
#   plot_layout(guides = 'collect') &
#   theme(legend.position = "right")
# # ggsave(plot=wrap.plot202, filename=paste0(fig_dir, "PDP_SKATER20/D202_REDUCED_Bivariate_PDP_wrapLegendRight_reduceLabs.png"), width=70, height=45, limitsize=FALSE)
# ggsave(plot=wrap.plot202, filename=paste0(fig_dir, "PDP_SKATER20/D202_REDUCED_Bivariate_PDP_wrapLegendRight_reduceLabs.png"), width=40, height=32, limitsize=FALSE)
