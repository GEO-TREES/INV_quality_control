# # find stems with missing pom
# stems_pom_missing <- stems_all %>% 
#   group_by(plot_id, stem_id) %>% 
#   filter(any(is.na(pom)))
# 
# stems_pom_split <- split(stems_pom_missing, 
#   paste(stems_pom_missing$plot_id, stems_pom_missing$stem_id))
# 
# # for each stem
# pom_fix <- bind_rows(mclapply(stems_pom_split, function(x) {
# 
#   # create vector to hold method of imputation
#   method <- rep(na_character_, length(x$pom))
# 
#   # if any poms are na, but some are not
#   if (any(is.na(x$pom)) & any(!is.na(x$pom))) {
# 
#     # compute run length encoding, excluding nas
#     x_rle_nona <- rle(c(na.omit(x$pom)))
# 
#     # compute run length encoding, with nas
#     x_rle <- rlena(x$pom)
# 
#     # for each run of na values 
#     for (i in which(is.na(x_rle$values))) {
#       # if first element
#       if (i == 1) {
#         # extrapolate backwards 
#         x_rle$values[i] <- x_rle$values[i+1]
#         method[seq(sum(x_rle$lengths[1:(i-1)])+1,
#           sum(x_rle$lengths[1:i]))] <- "extrapolated backwards"
#       # if last element
#       } else if (i == length(x_rle$values)) {
#         # extrapolate forwards 
#         x_rle$values[i] <- x_rle$values[i-1]
#         method[seq(sum(x_rle$lengths[1:(i-1)])+1,
#           sum(x_rle$lengths[1:i]))] <- "extrapolated forwards"
#       # if not first or last element
#       # if preceded and followed by same pom, assume same pom for na
#       } else if (x_rle$values[i-1] == x_rle$values[i-1]) {
#         x_rle$values[i] <- x_rle$values[i-1]
#         method[seq(sum(x_rle$lengths[1:i-1])+1,
#           sum(x_rle$lengths[1:i]))] <- "interpolated"
#       # otherwise, fill with previous value
#       } else {
#         x_rle$values[i] <- x_rle$values[i-1]
#         method[seq(sum(x_rle$lengths[1:i-1])+1,
#           sum(x_rle$lengths[1:i]))] <- "previous value"
#       }
# 
#       # reverse run length encoding to get vector with interpolated values 
#       x$pom <- inverse.rle(x_rle)
#     }
#   # if no pom information
#   } else if (all(is.na(x$pom))) {
#     # fill all with plot default pom
#     x$pom <- unique(x$default_pom)
#     method <- "no pom info, plot default pom"
#   }
# 
#   # return dataframe with adjusted pom and type of change
#   data.frame(
#     row_id = x$row_id,
#     pom_imput = x$pom,
#     pom_imput_method = method)
# }, mc.cores = detectcores()-1))

