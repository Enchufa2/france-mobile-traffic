ccv.glmnet <- function(formula, data, weights, family="gaussian", lambda="lambda.1se", ...) {
  mf <- match.call(expand.dots = FALSE)
  m <- match(c("formula", "data", "weights"), names(mf), 0L)
  mf <- mf[c(1L, m)]
  mf$drop.unused.levels <- TRUE
  mf[[1L]] <- quote(stats::model.frame)
  mf <- eval(mf, parent.frame())
  y <- model.response(mf, "numeric")
  w <- as.vector(model.weights(mf))
  mt <- attr(mf, "terms")
  x <- model.matrix(mt, mf)
  z <- if (is.null(w)) glmnet::cv.glmnet(x[,-1], y, family=family, ...)
  else glmnet::cv.glmnet(x[,-1], y, w, family=family, ...)
  z$x <- x
  z$y <- y
  z$w <- w
  z$terms <- mt
  z$choice <- lambda
  class(z) <- c("ccv.glmnet", class(z))
  z
}

fitted.ccv.glmnet <- function(object, ...) {
  drop(predict(object, object$x[,-1], s=object$choice, type="response"))
}

perf <- function(object, ...) UseMethod("perf")
perf.ccv.glmnet <- function(object, plot=TRUE, size=.3, shape=16, ...) {
  x <- fitted(object)
  y <- object$y
  w <- if (!is.null(object$w)) object$w / max(object$w) else rep(1, length(y))
  
  df <- if (isTRUE(ncol(y) > 1)) {
    do.call(rbind, lapply(1:ncol(y), function(i)
      data.frame(predicted=x[,i], observed=y[,i], weights=w, response=colnames(y)[i])))
  } else {
    data.frame(predicted=x, observed=y, weights=w)
  }
  
  if (!plot) return(df)
  
  p <- ggplot(df) + aes(predicted, observed, alpha=weights, weight=weights) +
    xlim(0, max(df$observed)) + ylim(0, max(df$observed)) +
    geom_abline(intercept=0, slope=1, col="lightgray") + geom_point(size=size, shape=shape) +
    geom_smooth(method="lm") + ggpmisc::stat_poly_eq(
      aes(label=paste(..adj.rr.label..)), formula=y~x, rr.digits=2, parse=TRUE,
      label.x="right", label.y="bottom") +
    labs(x="Predicted response", y="Observed response") +
    theme(legend.position="none")
  
  if (!is.null(df$response))
    p <- p + facet_wrap("response")
  p
}

std_coef <- function(object, ...) UseMethod("std_coef")
std_coef.ccv.glmnet <- function(object, plot=TRUE, ...) {
  coef_to_df <- function(x) {
    df <- data.frame(
      names=gsub("`", "", names(x)), coefficients=x, stringsAsFactors=FALSE)
    df$sign <- sign(df$coefficients)
    df$signf <- factor(df$sign, levels=c(-1, 1), labels=c("Negative", "Positive"))
    # standardize, remove intercept, reorder
    df$coefficients <- abs(df$coefficients) * apply(object$x, 2, sd) / sd(object$y)
    df <- df[-1,]
    df <- df[order(df$coefficients, decreasing=TRUE),]
    df
  }
  
  x <- coef(object, object$choice)
  df <- if (is.list(x)) {
    as.data.frame(rbindlist(lapply(x, function(i) coef_to_df(i[,1])), idcol="response"))
  } else {
    coef_to_df(x[,1])
  }
  
  df$font <- ifelse(grepl("^P15_POP", df$names), "plain", "bold")
  
  df$names <- sub("P15_POP_IMM", "Immigrant population", df$names)
  pop <- grepl("^P15_POP", df$names)
  df$names[pop] <- sub("^P15_POP", "", df$names[pop])
  df$names[pop] <- sapply(strsplit(df$names[pop], ""), function(x) 
    paste0("Population ", paste0(x[1:2], collapse=""), "-", paste0(x[-(1:2)], collapse="")))
  
  if (!plot) return(df)
  std_coef(df, ...)
}
std_coef.data.frame <- function(object, lim=NULL, size=4, names=NULL, lasso=TRUE, ...) {
  df <- object
  
  lim <- if (!is.null(lim))
    ylim(0, lim)
  else scale_y_continuous(expand=expand_scale(add=c(0, .025)))
  
  if (is.null(df$response)) {
    df$lasso <- df$coefficients == 0
    if (lasso) {
      p <- ggplot(df) + facet_grid("lasso", scales="free", space="free") +
        geom_rect(aes(x=NULL, y=NULL), data=data.frame(lasso=TRUE), 
                  xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf, fill="lightgray", alpha=0.5)
    } else {
      p <- ggplot(subset(df, !lasso))
    }
    p + aes(forcats::fct_rev(forcats::fct_inorder(names)), coefficients, fill=signf) +
      geom_col() + lim + coord_flip() + scale_fill_discrete(na.translate=FALSE) +
      geom_text(aes(label=names, fontface=font), hjust=0, nudge_y=.005, size=size) + 
      labs(y="Standardized coef. magnitude", x=NULL, fill="Effect") +
      theme(axis.line.y=element_blank(), axis.ticks.y=element_blank(), 
            axis.text.y=element_blank(),
            strip.background=element_blank(), strip.text=element_blank(),
            legend.position=c(1, if (lasso) 0.5 else 0.01),
            legend.justification=c(1, if (lasso) 0.5 else 0),
            panel.grid.major.x=element_line(color="lightgray"))
  } else {
    if (!is.null(names))
      df <- df[df$names %in% names,]
    levels <- subset(df, response==response[[nrow(df)]])$names
    df$names <- factor(df$names, levels=levels)
    ggplot(df) +
      aes(forcats::fct_inorder(response), coefficients, group=names) +
      geom_line(aes(color=names), show.legend=FALSE) + 
      geom_point(size=5, colour=theme_get()$panel.background$fill) +
      geom_point(aes(color=names), show.legend=FALSE) +
      ggrepel::geom_label_repel(aes(label=names, fill=signf), 
                                data=subset(df, response==response[[nrow(df)]]),
                                hjust=0, nudge_x=.2, size=size) +
      scale_x_discrete(expand=expand_scale(mult=c(0.07, 0.35)), labels=1:9) +
      scale_color_viridis_d() +
      labs(y="Standardized coef. magnitude", x=NULL, color="Traffic category", fill="Effect") +
      theme(axis.line.x=element_blank(), axis.ticks.x=element_blank(),
            legend.position=c(1, 0.01), legend.justification=c(1, 0),
            panel.grid.major.x=element_line(color="lightgray"))
  }
}
