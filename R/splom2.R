#' Modified lattice::splom.
#' 
#' Upper panel computes correlation, and lower panel adds regression line.
#' 
#' @param x,grid,pscale,type,col.line,... Same as that in \code{\link[lattice]{splom}}.
#' @param noi Panels in row \code{i} are not plotted.
#' @param noj Panels in column \code{j} are not plotted.
#' @examples
#' \donttest{
#' splom2(~iris[1:4])
#' splom2(~iris[1:4],noj=4,noi=4)
#' }

#' @export
splom2=function(x,grid=T,pscale=0,type=c("r","g","p"),col.line=2,noi=NULL,noj=noi,...){
  require(lattice,quietly = TRUE)
  splom(x,
      upper.panel = function(x, y,i,j, ...) {
        if((!i%in%noi)&(!j%in%noj)){
          # panel.fill(col = brewer.pal(9, "RdBu")[ round(cor(x, y) * 4 + 5)])
          cpl <- current.panel.limits()
          panel.text(mean(cpl$xlim), mean(cpl$ylim), round(cor(x, y,use="complete"),2), font=2,
                     adj=c(0.5,-0.6))
          panel.text(mean(cpl$xlim), mean(cpl$ylim), paste0("p=",round( cor.test(x,y)$p.value, 2)), font=1,
                     adj=c(0.5,0.6),col='blue')
        }
      },
      lower.panel=function(x,y,i,j,...){
        if((!i%in%noi)&(!j%in%noj)){
          panel.splom(x,y,...)
        }
      },grid=grid,pscale=pscale,type=type,col.line=col.line,...)
}