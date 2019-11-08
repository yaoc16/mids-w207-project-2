# Set some global options
options(rgl.useNULL=TRUE) # must be run before library(rgl)
options(scipen=10)
options(repr.matrix.max.cols=74)


# Load some libraries
suppressMessages(library(plyr))
suppressMessages(library(dplyr))

suppressMessages(library(base64enc))
suppressMessages(library(caret))
suppressMessages(library(dummies))
suppressMessages(library(e1071))
suppressMessages(library(entropy))
suppressMessages(library(ggdendro))
suppressMessages(library(ggplot2))
suppressMessages(library(ggwordcloud))
suppressMessages(library(gridExtra))
suppressMessages(library(htmltools))
suppressMessages(library(IRdisplay))
#suppressMessages(library(kableExtra))
suppressMessages(library(kknn))
suppressMessages(library(ks))
suppressMessages(library(knitr))
suppressMessages(library(lubridate))
suppressMessages(library(MASS))
suppressMessages(library(mclust))
suppressMessages(library(mvtnorm))
suppressMessages(library(neuralnet))
suppressMessages(library(NLP))
suppressMessages(library(pracma))
suppressMessages(library(polyclip))
suppressMessages(library(psych))
suppressMessages(library(readxl))
suppressMessages(library(reshape2))
suppressMessages(library(rgl))
suppressMessages(library(rmarkdown))
suppressMessages(library(rpart))
suppressMessages(library(rpart.plot))
suppressMessages(library(SnowballC))
suppressMessages(library(tm))


# Set some visualization constants
PALETTE = rep(c("#00A9FF", "tomato", "#7CAE00", "#C77CFF", "goldenrod2", "chartreuse4", "salmon", "bisque4"),10)
NEW_COLOR = "grey60"
guides.standard = guides(color=guide_legend(override.aes=list(size=2, alpha=1)))
theme.x_axis_only = theme(axis.title.y=element_blank(), axis.text.y=element_blank(), axis.ticks.y=element_blank(), panel.grid.major.y=element_blank(), panel.grid.minor.y=element_blank())
theme.y_axis_only = theme(axis.title.x=element_blank(), axis.text.x=element_blank(), axis.ticks.x=element_blank(), panel.grid.major.x=element_blank(), panel.grid.minor.x=element_blank())
theme.x_axis_45 = theme(axis.text.x=element_text(angle=45, hjust=1, vjust=1))
theme.no_axes = theme.x_axis_only + theme.y_axis_only
theme.no_axis_titles = theme(axis.title.x=element_blank(), axis.title.y=element_blank())
theme.no_x_axis_title = theme(axis.title.x=element_blank())
theme.legend_title = theme(legend.title=element_text(size=7))
theme.legend_below = theme(legend.position="bottom")
theme.no_legend = theme(legend.position="none")
standard_arrow = arrow(angle=15, type="closed", length=unit(10, "points"))
restore = NULL


# Define some useful functions

as.binary = function(v, s) { as.numeric(v == s) }

as.class = function(prob, class, cutoff=NA) { f = factor(prob[,colnames(prob)==class]>=cutoff, levels=c(TRUE, FALSE), labels=c(class, setdiff(colnames(prob), class))); attr(f, "names") = NULL; f }

as.covariance_matrix = function(sd, correlation) { matrix(c(sd[1]^2, prod(sd)*correlation, prod(sd)*correlation, sd[2]^2), 2, 2) }

as.data.frame.content = function(x) { d = as.data.frame(unlist(lapply(1:length(x), function(i) x[[i]]$content))); colnames(d) = "V1"; d }

exhaustive = function(v, keep="class") { v = v[!v %in% keep]; unlist(lapply(1:length(v), function(j) lapply(j:length(v), function(i) c(v[j:i], keep))), recursive=FALSE) }

fmt = function(x, title=NULL, row.names=FALSE)
  { if (class(x) %in% c("data.frame","matrix","table")) x.fmt = if (row.names) data.frame(x) else data.frame(x, row.names=NULL)
    else
      { if (is.null(title)) title = as.character(sys.call(sys.parent(6)))[2]
        x.fmt = if (row.names) data.frame(x) else data.frame(x, row.names=NULL); colnames(x.fmt) = title }
    x.fmt }

caption = function(x, title=NULL, position=NULL, row.names=FALSE)
  { if (is.null(title)) title = as.character(sys.call(sys.parent(6)))[2]
    if (is.null(position)) position = "center"
    x.fmt = as.character(kable(x, format="html", align=rep("r", ncol(x)), caption=paste0("<", position, "><font color=\"black\"><b>", title, "</b></font></", position, ">"), row.names=row.names))
    display_html(x.fmt) }

captionx = function(x, title=NULL, position=NULL, row.names=FALSE)
  { if (is.null(title)) title = as.character(sys.call(sys.parent(6)))[2]
    if (is.null(position)) position = "center"
    x.fmt = as.character(kable(x, format="html", align=rep("r", ncol(x)), caption=paste0("<", position, "><font color=\"black\"><b>", title, "</b></font></", position, ">"), row.names=row.names)) }

custom_name_repair = function(x) { x = gsub("\\((.+)\\)", "\\.\\1", x)
                                   x = gsub(" ", "\\.", x)
                                   x = gsub("@", "", x)
                                   x }

dummify = function(...) suppressWarnings(dummy.data.frame(...))

fmt.dm = function(dm) { d = as.data.frame(as.matrix(dm))
                        d1 = adply(1:ncol(d), 1, function(j) sprintf("%0.4f", d[,j]))
                        d1 = d1[, -1]
                        row.names(d1) = row.names(d)
                        names(d1) = names(d)
                        d1 }

focus_data = function(data, hit, emphasis=10) { prob.x = as.numeric(as.character(factor(hit, levels=c(TRUE, FALSE), labels=c(1,emphasis))))
                                                prob = prob.x / sum(prob.x)
                                                data[sample(1:nrow(data), replace=TRUE, prob=prob),] }

# front = function(x) c(tail(x,1), head(x,-1))
front = function(d) { x = colnames(d); d[, c(tail(x,1), head(x,-1))] }

# gaussian = function(x, mean, sd) { (1/(sd*sqrt(2*pi))) * exp(-0.5 * ((x-mean)/sd)^2) }
gaussian = function(x, mean, sd, size=1) { size * (1/(sd*sqrt(2*pi))) * exp(-0.5 * ((x-mean)/sd)^2) }

# gaussian2 = function(grid, mean, cm) { aaply(1:nrow(grid), 1, function(i) { dmvnorm(c(grid[i,1],grid[i,2]), mean=mean, sigma=cm) } ) }
gaussian2 = function(grid, mean, cm, size=1) { size * aaply(1:nrow(grid), 1, function(i) { dmvnorm(c(grid[i,1],grid[i,2]), mean=mean, sigma=cm) } ) }

get.cor = function(cm) { cm[1,2] / sqrt(cm[1,1]) / sqrt(cm[2,2]) }

get.sd = function(cm) { c(sqrt(cm[1,1]), sqrt(cm[2,2])) }

ggtree = function(model) { ggplot() + 
                           geom_segment(aes(x=x, y=y, xend=xend, yend=yend), dendro_data(model)$segments) + 
                           geom_label(aes(x=x, y=y, label=label), dendro_data(model)$labels, size=2.5) +
                           geom_label(aes(x=x, y=y, label=label, fill=label), dendro_data(model)$leaf_labels, size=2.5) +
                           theme.no_axes + theme.no_legend }

impute.factor = function(v, iv) { factor(aaply(as.character(v), 1, function(x) if (is.na(x)) as.character(iv) else x), levels(v)) }
impute.numeric = function(v, iv) { aaply(v, 1, function(x) if (is.na(x)) iv else x) }
impute = function(d)
  { l = alply(1:ncol(d), 1, function(j) if (class(d[,j])[1]=="numeric") impute.numeric(d[,j], mean(d[,j], na.rm=TRUE))
                                        else                            impute.factor(d[,j], mode(d[,j])) )
    dx = as.data.frame(l)
    names(dx) = names(d)
    dx }

inside = function(point, polygon)
  { P = list(x=point[,1], y=point[,2])
    A = list(x=polygon[,1], y=polygon[,2])
    as.logical(abs(pointinpolygon(P,A, eps=0.001))) }

kernel.gauss = function(x, mean, sd) { (1/(sd*sqrt(2*pi))) * exp(-0.5 * ((x-mean)/sd)^2) / 11 }

kernel.rect = function(x, center, halfwidth) { z=rep(0, length(x)); z[(x >= center-halfwidth) & (x <= center+halfwidth)] = 1 / (11*halfwidth); z }

kernel.tri = function(x, center, halfwidth)
  { hw = halfwidth;
    z = rep(0,length(x));
    i = which((x >= center) & (x <= center+hw));
    z[i] = aaply(i, 1, function(ii) {(((center+hw)-x[ii])/hw)*(1/hw)} );
    j = which((x >= center-hw) & (x < center));
    z[j] = aaply(j, 1, function(jj) {((x[jj]-(center-hw))/hw)*(1/hw)} );
    z = z/11 
    z }

lookahead = function(d, k, suffix=".LA")
  { h = d[(k+1):nrow(d), ]
    t = as.data.frame(matrix(NA, k, ncol(d))); names(t) = names(d)
    r = rbind(h, t); names(r) = paste0(names(d), suffix, k)
    r }
	
lookback = function(d, k, suffix=".LB")
  { h = as.data.frame(matrix(NA, k, ncol(d))); names(h) = names(d) 
    t = d[1:(nrow(d)-k), ]
    r = rbind(h, t); names(r) = paste0(names(d), suffix, k)
    r }

mandelbrot = function(xmin=-2, xmax=1, ymin=-1.5, ymax=1.5, resolution=500)
  { x = seq(xmin, xmax, length.out=resolution)
    y = seq(ymin, ymax, length.out=resolution)
    z = matrix(0.0, nrow=resolution, ncol=resolution)
    k = matrix(0.0, nrow=resolution, ncol=resolution)
    c = outer(x, y*1i, FUN="+")
    for (rep in 1:50) { i = Mod(z) < 2; z[i] = z[i]^2 + c[i]; k[i] = k[i] + 1 }
    m = list(); m[["matrix"]] = k; m[["x"]] = x; m[["y"]] = y
    m }

mode = function(v) { tbl = as.data.frame(table(v)); tbl[tbl[,2]==max(tbl[,2]),1][1] }

my_predict = function(model, data) { p = if (sum(data[,1])>15) c(0.8,0.3,0.6,0.2,0.8) else if (sum(data[,1])>10) c(0.6,0.9,0.6,0.1,0.1) else c(0.4,0.3,0.6,0.2,0.7); p = p[1:nrow(data)]; d = data.frame(p, 1-p); colnames(d) = attr(model, "vars"); d }

my_predictive_analytic_method = function(formula, data, hyperparameter) { x="A model constructed by my predictive analytic method"; attr(x, "vars") = unique(data$class); x }

mystery_process_A = c(0, 0.5, 0.5, 0, 0, 0)

mystery_process_B.function = function(x) { (-3.3003730075023 + 25.7489500997727*x + -14.8400555259565*x^2 + 3.15566230978222*x^3 + -0.308590022994962*x^4 + 0.014011451172589*x^5 + -0.000238775338979748*x^6 + 10) / 232.881735467096} 

mystery_process_B = c(aaply(0:19, 1, function(x) integrate(mystery_process_B.function,x,x+1)$value ), 0)

output_size = function(width=8, height=3) options(repr.plot.width=width, repr.plot.height=height)

pc_constituents = function(pc)
  { pc_names = names(as.data.frame(pc$rotation))
    qual = as.data.frame(t(adply(pc_names, 1, function(j) row.names(pc$rotation)[rev(order(abs(pc$rotation[,j])))])[-1]))
    names(qual) = pc_names
    row.names(qual) = NULL
    qual }

#pick = function(d, fun, labels) { factor(aaply(1:nrow(d), 1, function(i) { min(which(d[i,]==fun(d[i,]))) } ), labels=labels) }
#pick = function(v, f) { if (class(v) %in% c("data.frame", "matrix")) factor(colnames(v)[v == f(v)], levels=colnames(v)) else names(prob)[prob == max(prob)] }
pick = function(d, f, labels=colnames(d)) { factor(aaply(d, 1, function(v) colnames(d)[v == max(v)], .expand=FALSE), levels=colnames(d), labels=labels) }

plain_var_names = function (d) { colnames(d) = gsub(" ", "_", colnames(d)); d }

regulate_columns = function(new.t, data.t) { i = intersect(colnames(data.t), colnames(new.t))
                                             j = setdiff(colnames(data.t), intersect(colnames(data.t), colnames(new.t)))
                                             d = as.data.frame(matrix(rep(0, nrow(new.t)*length(j)), nrow=nrow(new.t)))
                                             colnames(d) = j
                                             x = cbind(new.t[,i], d)
                                             x = x[,sort(colnames(x))]
                                             x }

remove_na_rows = function(d) d[as.logical(alply(data.jobs, 1, function(x) any(aaply(1:6, 1, function(j) !(is.na(x[1,j])))))),]

removeSpecialChars = content_transformer(function(x, chars) gsub(paste("[", chars, "]", sep=""), "", x))

round_robin = function(...) { tt = table(as.character(lapply(lapply(list(...), pick, max), as.character)))
                              factor(names(tt)[which.max(tt)], levels=unique(unlist(lapply(list(...), colnames)))) }

row.arrange = function(...)
  { data = list(...)
    td = ""
    for (i in 1:length(data)) 
      { if (class(data[[i]])=="data.frame") data[[i]] = as.character(kable(data[[i]], format="html")) 
        td = paste0(td, "<td style=\"background-color:white; vertical-align:top;", 
                    if (i<length(data)) " padding-right:40px;" else "",
                    "\">", data[[i]], "</td>") }
    display_html(paste0("<table><tr>", td, "</tr></table>")) }

sd_columns = function(data) aaply(1:ncol(data), 1, function(j) sd(data[,j]))

select_if.index = function(data, f, ...) as.vector(which(aaply(names(data), 1, function(j) f(data[, j], ...))))              

select_if.name = function(data, f, ...) names(data)[aaply(names(data), 1, function(j) f(data[, j], ...))]

shift_down = function(v, i) { c(v[(i+1):length(v)], rep(NA, i)) }

shift_up = function(v, i) { c(rep(NA, i), v[1:(length(v)-i)])  }

size = function(data) data.frame(observations=dim(data)[1], variables=dim(data)[2])

stretch = function(values, n, each=1, last=NULL) { values.n = rep(values, each=each, length.out=n)
                                                   if (!is.null(last)) values.n[length(values.n)] = last 
                                                   values.n }

sum_kernels.tri = function(x, x1, halfwidth) { aaply(x, 1, function(i) { sum(aaply(x1, 1, function(j) kernel.tri(i, j, halfwidth))) } ) }

sum_kernels.gauss = function(x, x1, sd) { aaply(x, 1, function(i) { sum(aaply(x1, 1, function(j) kernel.gauss(i, j, sd))) } ) }

sum_kernels.rect = function(x, x1, halfwidth) { aaply(x, 1, function(i) { sum(aaply(x1, 1, function(j) kernel.rect(i, j, halfwidth))) } ) }

toc = function(d, indent=0, html=TRUE, level=1)
  { space = if (html) "&nbsp;" else " "
    newline = if (html) "<br/>" else "\n"
    s = split(d, d[,1], drop=TRUE)
    x = ""  
    for (i in names(s))
      { x = paste0(x, paste(rep(space,indent), collapse=""), paste0(if (level==1) newline else "", i), newline)
        if (nrow(s[[i]])>1) x = paste0(x, toc(s[[i]][,-1,drop=FALSE], indent+4, html, level+1)) }
    return(x) }

unibigrams = function(x) unlist(lapply(ngrams(words(x), c(1,2)), paste, collapse=" "))                                                                    

var_columns = function(data) aaply(1:ncol(data), 1, function(j) var(data[,j]))                                   

vote = function(...) { x = list(...); factor(levels(unlist(x))[which.max(table(unlist(x)))], levels=levels(unlist(x))) }

youtube = function(id, width=500, height=500) { display_html(paste0('<iframe src =https://www.youtube.com/embed/', id, ' width=', width, ', height=', height, ' allowfullscreen></iframe> ' )) }

zoom = function(polygon, xlim=c(0,1), ylim=c(0,1))
  { mx = 0.001*abs(xlim[2]-xlim[1])
    my = 0.001*abs(ylim[2]-ylim[1])
    A = list(x=polygon[,1], y=polygon[,2])
    B = list(x=xlim[c(1,2,2,1)]+c(mx,-mx,-mx,mx), y=ylim[c(1,1,2,2)]+c(my,my,-my,-my))
    X = polyclip(A, B, op="intersection")
    polygon.zoom = as.data.frame(X[[1]]); names(polygon.zoom) = names(polygon)
    polygon.zoom }
	

# Set some visualization formatting defaults
output_size(restore)
update_geom_defaults("abline",     list(color="black", size=0.15))
update_geom_defaults("area",       list(color=NA, fill=PALETTE[1]))
update_geom_defaults("bar",        list(color=NA, fill=PALETTE[1], lwd=5))
update_geom_defaults("col",        list(fill=PALETTE[3], color=NA, lwd=5))
update_geom_defaults("density",    list(size=3, fill=PALETTE[1], colour=NA))
update_geom_defaults("density_2d", list(size=0.1, colour="black"))
update_geom_defaults("hline",      list(color="black", size=0.15))
update_geom_defaults("label",      list(colour="black"))
update_geom_defaults("line",       list(size=0.5, colour=PALETTE[1]))
update_geom_defaults("path",       list(size=0.5, colour=PALETTE[1]))
update_geom_defaults("point",      list(size=3, colour=PALETTE[1]))
update_geom_defaults("segment",    list(size=0.5, colour=PALETTE[1]))
update_geom_defaults("smooth",     list(color="black", size=0.15))
update_geom_defaults("text",       list(size=2.5, vjust=-1.5))
update_geom_defaults("violin",     list(color=NA, lwd=5))
update_geom_defaults("vline",      list(color="black", size=0.15))
#update_geom_defaults("vline",      list(size=0.1, colour="black"))
scale_colour_discrete = function(...) scale_colour_manual(..., values=PALETTE)
scale_fill_discrete   = function(...) scale_fill_manual(..., values=PALETTE)
theme_update(plot.title=element_text(size=10, hjust=0.5), plot.subtitle=element_text(size=8, face="italic", hjust=0.5), axis.title=element_text(size=7), axis.text=element_text(size=7), strip.text=element_text(size=7), strip.text.y=element_text(angle=90), legend.title=element_blank(), legend.text=element_text(size=7))


"== document setup ==" %>% display_html