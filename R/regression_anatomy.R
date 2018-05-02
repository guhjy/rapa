example.regression.anatomy = function() {
  setwd("D:/libraries/rapa")
  dat = foreign::read.dta("intdata.dta")

  library(dplyr)
  dat = mutate(dat, log_top1 = log(top1), log_mtr = log(mtr), Year = as.factor(year))# %>% filter(year == 2005)
  reg1 = lm(log_top1 ~ log_mtr, data=dat)
  reg2 = lm(log_top1 ~ log_mtr + gdpcap, data=dat)

  yvar = "log_top1"
  cvar = c("log_mtr","Year")
  xvar = "gdpcap"

  dat = na.omit(dat)

  dat$org_y = dat[[yvar]]
  df = add.tilde.xy(dat,yvar, xvar, cvar)




  ggplot(data=df, aes_string(x=xvar, y=yvar, color="country")) + geom_point() + geom_smooth(method="lm", color="black") + facet_wrap(~.control)


  ggplot(data=df, aes_string(x=xvar, y=yvar, color="org_y")) + geom_point() + geom_smooth(method="lm", color="black") + facet_wrap(~.control) + scale_colour_gradient()

  plot()

  yreg = lm(log_top1 ~ gdpcap)

  dat$y.tilde = resid()

  gg = ggplot()



}

simpsons.example = function() {
  dat = mosaicData::SAT

  # Create state abbreviations
  setNames(state.abb, state.name)
  dat$code = state.abb[dat$state]


  xvar = "sat"
  cvar = c("frac")
  yvar = "salary"

  dat = na.omit(dat)

  dat$org_x = dat[[xvar]]

  df = add.tilde.xy(dat,NULL, yvar, xvar, cvar, adjust.xy="x")


  ggplot(data=df, aes_string(x=xvar, y=yvar, color=".dist.x", label="code")) + geom_text() + geom_smooth(method="lm") + facet_wrap(~.control) + scale_colour_gradient2(low="#ff0000", high="#0000ff", mid="#000000", midpoint = 0) + theme_bw()

  d = filter(df, !(.adjust=="xy" & .control.ind==0))

  p = ggplotly(
    ggplot(data=d, aes_string(x=xvar, y=yvar, color="org_x", label="code", frame=".frame")) + geom_text() + geom_smooth(method="lm", se=FALSE) + scale_colour_gradient2(low="#ff0000", high="#0000ff", mid="#000000", midpoint = mean(range(df$org_x))) + theme_bw()

  ) %>% config(displayModeBar = F) %>% animation_opts(frame = 3000, transition = 3000, easing = "linear", redraw = FALSE, mode = "immediate") %>% animation_button()

  p = p %>% animation_slider(currentvalue = list(prefix = "", font = list(color="white")))
  p

  devtools::install_github('hadley/ggplot2', dep=FALSE)
}



rapa.data = function(dat, formula=NULL, yvar=NULL, xvar=NULL, cvars=NULL,ivars=NULL, frames=NULL,  org.x.col = ".org.x", dist.x.col=".dist.x", frame.col=".frame", frame.ind.col=".frame.ind", control.ind.col = ".control.ind", control.col = ".control", adjust.col = ".adjust", control.label = paste0("control ", seq_along(cvars), ": ", cvars), no.control.label = "no control", adjust.control = c("sim","seq")[1], adjust.xy=c("seq","sim","x")[1], duplicate.org=FALSE) {
  restore.point("add.tilde.xy")

  if (!is.null(formula)) {
    vars = all.vars(formula)
    yvar = vars[1]
    xvar = vars[2]
    cvars = vars[-c(1:2)]
  }

  if (adjust.xy == "x") yvar = NULL
  if (is.null(yvar)) adjust.xy = "x"


  if (is.null(frames)) {
    frames = "original"
    if (duplicate.org) frames = c(frames,"original2")

    if (adjust.control=="sim") {
      if (adjust.xy == "seq") {
        frames = c(frames,"control x","control y")
      } else if (adjust.xy == "sim") {
        frames = c(frames,"control")
      } else if (adjust.xy == "x") {
        frames = c(frames, "control x")
      } else {
        stop("Unknown adjust.xy argument.")
      }
    } else if (adjust.control=="seq") {
      if (adjust.xy == "seq") {
        grid = expand.grid(c("x","y"), cvars)
        frames = c(frames,paste0("control ", grid[,1], " ", grid[,2]))
      } else if (adjust.xy == "sim") {
        frames = c(frames,paste0("control ", cvars))
      } else if (adjust.xy == "x") {
        frames = c(frames,paste0("control x", cvars))
      } else {
        stop("Unknown adjust.xy argument.")
      }
    } else {
      stop("Unknown adjust.control argument.")
    }
    frames = paste0(seq_along(frames), ". ", frames)
  }


  li = vector("list", length(frames))

  i = 1
  dat[[frame.col]] = frames[i]
  dat[[frame.ind.col]] = i
  dat[[control.col]] = no.control.label
  dat[[control.ind.col]] = 0
  dat[[org.x.col]] = dat[[xvar]]
  dat[[dist.x.col]] = 0
  if (adjust.xy == "sim") {
    dat[[adjust.col]] = "xy"
  } else {
    dat[[adjust.col]] = "x"
  }
  li[[i]] = dat

  if (adjust.xy == "seq" & duplicate.org) {
    i = i +1
    dat[[adjust.col]] = "xy"
    dat[[frame.col]] = frames[i]
    dat[[frame.ind.col]] = i
    li[[i]] = dat
  }


  if (adjust.control == "seq") {
    for (cvar.ind in seq_along(cvars)) {
      cvar = cvars[cvar.ind]
      d = dat

      dat[[xvar]] = anatomy.tilde.value(dat[[xvar]],dat[[cvar]])
      dat[[control.col]] = control.label[cvar.ind]
      dat[[control.ind.col]] = cvar.ind
      dat[[dist.x.col]] = dat[[xvar]] - dat[[org.x.col]]

      if (adjust.xy == "sim") {
        dat[[yvar]] = anatomy.tilde.value(dat[[yvar]],dat[[cvar]])
        dat[[adjust.col]] = "xy"
      } else {
        dat[[adjust.col]] = "x"
      }
      i = i +1
      dat[[frame.col]] = frames[i]
      dat[[frame.ind.col]] = i
      li[[i]] = dat

      if (adjust.xy == "seq") {
        dat[[yvar]] = anatomy.tilde.value(dat[[yvar]],dat[[cvar]])
        i = i +1
        dat[[frame.col]] = frames[i]
        dat[[frame.ind.col]] = i
        dat[[adjust.col]] = "xy"
        li[[i]] = dat
      }
    }
  # Add simultaneously all controls
  } else {
    d = dat

    dat[[xvar]] = anatomy.tilde.value(dat[[xvar]],dat[cvar])
    dat[[control.col]] = "control"
    dat[[control.ind.col]] = 1
    dat[[dist.x.col]] = dat[[xvar]] - dat[[org.x.col]]

    if (adjust.xy == "sim") {
      dat[[yvar]] = anatomy.tilde.value(dat[[yvar]],dat[cvar])
      dat[[adjust.col]] = "xy"
    } else {
      dat[[adjust.col]] = "x"
    }
    i = i +1
    dat[[frame.col]] = frames[i]
    dat[[frame.ind.col]] = i

    li[[i]] = dat

    if (adjust.xy == "seq") {
      dat[[yvar]] = anatomy.tilde.value(dat[[yvar]],dat[cvar])
      i = i +1
      dat[[frame.col]] = frames[i]
      dat[[frame.ind.col]] = i
      dat[[adjust.col]] = "xy"
      li[[i]] = dat
    }
  }
  df = do.call(rbind, li)

  levels = c(no.control.label,control.label)
  df[[control.col]] = ordered(df[[control.col]], levels)

  df[[frame.col]] = ordered(df[[frame.col]], frames)


  df

}


anatomy.tilde.value = function(val, control, add.mean = TRUE) {
  if (is.data.frame(control)) {
    has.factor = any(sapply(control, function(x) is.factor(x) | is.character(x)))
    if (!has.factor) {
      control = as.matrix(control)
    }
  } else {
    has.factor = is.factor(control) | is.character(control)
    if (has.factor) {
      control = data.frame(var=control)
    }
  }

  if (is.data.frame(control)) {
    formula = as.formula(paste0("val ~ ", paste0("control$", colnames(control), collapse=" + ")))
    tilde = resid(lm(formula))
  } else {
    tilde = resid(lm.fit(y = val,x = cbind(1,control)))
  }

  if (add.mean) tilde = tilde + mean(val)
  return(tilde)
}

regression.anatomy.x.tilde = function(mod,X=model.matrix(mod), add.mean = TRUE) {
  xdat = as.matrix(X)
  if (all(xdat[,1]==1)) xdat=xdat[,-1]

  x.tilde = xdat

  #dat = get.regression.data(mod)
  #xdat = as.matrix(dat[,-1])
  xvars = colnames(xdat)

  i = 1
  for (i in seq_along(xvars)) {
    x.tilde[,i] = resid(lm.fit(y = xdat[,i],x = cbind(1,xdat[,-i]))) + ifelse(add.mean,mean(xdat[,i]))
  }
  as.data.frame(x.tilde)
}


#' Regression anatomy plots
#'
#' Show original scatterplots and scatterplots
#' of y and controlled x_i, i.e. the residuals
#' of a regression of x_i on the other explanatory
#' variables. See e.g. Filoso (2010):
#' https://mpra.ub.uni-muenchen.de/42716/1/FILOSO-Regression_Anatomy_Revealed.pdf

#' @param mod A regression model, a result of a call to lm.
#' @param vars which x variables shall be included in the plot
#' @export
regression.anatomy.plots = function(mod, vars=NULL) {
  dat = get.regression.data(mod)
  xvars = colnames(dat)[-1]
  x.tilde = regression.anatomy.x.tilde(mod)
  tilde = cbind(dat[[1]],x.tilde)
  colnames(tilde) = colnames(dat)

  dat$TRANSFORM = "orginal"
  tilde$TRANSFORM = "controlled"

  yvar = colnames(dat)[1]
  d = rbind(dat, tilde)
  library(tidyr)

  dl = gather_(d,key_col="xvar",value_col="x", gather_cols=xvars)

  if (!is.null(vars)) {
    dl = filter(dl, xvar %in% vars)
  }

  library(ggplot2)
  ggplot(data=dl, aes_string(y=yvar,x="x",color="xvar")) +
    geom_point()+
    geom_smooth(method = lm,se = FALSE, color="black")+
    facet_grid(TRANSFORM~xvar,scales = "free_x")

}
