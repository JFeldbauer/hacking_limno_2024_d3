# paste into R console or run as part of script

if (! "remotes" %in% installed.packages()[,1])
  install.packages("remotes")
library("remotes")

remotes::install_github("dkneis/rodeo")

rm(list=ls())
library("rodeo")

m <- buildFromWorkbook("reactor_eqns_v1.xlsx")
print(class(m))

print("Current initial values:")
print( m$getVars() )

print("Current values of parameters:")
print( m$getPars() )

x <- m$dynamics(times=0:24, fortran=F)

print(head(x,3))
print(tail(x,3))

plt <- function(x, m) { # x: data, m: model obj.
  
  vn <- m$namesVars()   # variable names
  nv <- m$lenVars()     # number of variables
  
  matplot(x[,"time"], x[,vn], log="y", type="l",
          lty=1:nv, col=1:nv, xlab="Hour", ylab="Value")
  legend("right", bty="n", lty=1:nv,
         col=1:nv, legend=vn)
}

plt(x, m)

m <- buildFromWorkbook("reactor_eqns_v2.xlsx")

x <- m$dynamics(times=0:48, fortran=F)

print(x[1:3, 1:6])

plt(x, m)

p <- m$getPars()    # current settings
p["Ain"] <- 10      # now with antibiotic
m$setPars(p)        # update settings

# no need to rebuild/recompile the model
x <- m$dynamics(times=0:48, fortran=F)
plt(x, m)

Ain <- function(time) {
  h <- floor((time/24 - floor(time/24)) * 24)
  if (h %in% c(0:5))
    10
  else
    0
}

m <- buildFromWorkbook("reactor_eqns_v3.xlsx")

x <- m$dynamics(times=0:240, fortran=F)

plt(x, m)

library("rodeo")

ncells <- 100     # number of cells
fortran <- TRUE   # TRUE requires compiler !

# import equations and declarations,
# translate into source code, and compile
m <- buildFromWorkbook("river_eqns_v1.xlsx",
                       dim=ncells, fortran=fortran, na="NA")

# retrieve defaults from workbook
p <- m$getParsTable()
# save as vector 
p <- setNames(p$default, p$name)
# copy to all cells (vector -> matrix)
p <- sapply(p, rep, length.out=ncells)

print(p[1:2, 1:5])

# adjust mask parameters
p[,"is_upstr"] <- c(1, rep(0, ncells-1))
p[,"is_centr"] <- c(0, rep(1, ncells-1))
p[,"has_src"] <- rep(0, ncells)
p[round(ncells) / 10, "has_src"] <- 1

# assign parameters to model object
m$setPars(p)

v <- m$getVarsTable()
v <- setNames(v$default, v$name)
v <- sapply(v, rep, length.out=ncells)

m$setVars(v)

times <- c(0, 12, 24, 48, 96, 120)
x <- m$dynamics(times=times, fortran=fortran)

print(x[ ,1:7])

# dimension 1: time
# dimension 2: cell
# dimension 3: variables and process rates
a <- array(x[,colnames(x) != "time"],
           dim=c(nrow(x), ncells, sum(m$lenVars(),
                                      m$lenPros())),
           dimnames=list(time=x[,"time"], cell=1:ncells,
                         variable=c(m$namesVars(), m$namesPros()))
)

print(a["48", "10", "X"])

plot_longit <- function(v, a, leg) {
  nc <- dim(a)[names(dimnames(a)) == "cell"]
  plot(c(1, nc), range(a[,,v]),
       type="n", xlab="Cell", ylab="")
  times <- dimnames(a)[["time"]]
  for (t in times)
    lines(1:nc, a[t, , v], col=match(t, times))
  if (leg) legend("right", bty="n", title="Time",
                  lty=1, col=1:length(times), legend=times)
  abline(v=which(p[,"has_src"] == 1), lty=2)
  mtext(side=3, paste("Var.:",v), cex=par("cex"))
}

layout(matrix(1:3, nrow=1))

for (v in c("S", "X", "B")) {
  plot_longit(v, a, leg=(v=="S"))
}

layout(1)


m <- buildFromWorkbook(
  "river_eqns_v2.xlsx",
  dim=ncells, fortran=fortran, na="NA",
  sources="river_func.f95"
)




