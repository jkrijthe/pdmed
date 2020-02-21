library(sandwich)
library(lmtest)
library(stringr)

scale_2sd <- function(x) {
  scale(x, center = mean(x,na.rm=TRUE), scale = 2*sd(x, na.rm = TRUE))
}

theme_krijthe_gray <- function(base_size=12,base_family="sans") {
  theme(
    text = element_text(family=base_family,colour="black"),
    rect = element_rect(fill=NA),
    plot.background = element_rect(fill="#F0F0F0"),
    axis.line = element_line(colour="#D6D6D6",lineend=3),
    panel.background = element_rect(fill=NA),
    panel.grid.major = element_line(color="#D6D6D6"),
    panel.grid.minor = element_line(color=NA),
    plot.title = element_text(size=base_size*2,face="bold"),
    plot.margin = grid::unit(rep(0.5,4),"cm")
  )
} 

tidy.ipw <- function(mod,conf.int=TRUE) {
  df <- as_tibble(as.matrix(coeftest(mod,vcov. = vcovHC(mod, "HC1"))[,]))
  df$term <- names(mod$coefficients)
  df$estimate <- mod$coefficients
  df <- df %>% select(term,estimate,std.error=`Std. Error`,statistic=`t value`,p.value=`Pr(>|t|)`)
  df <- df %>% mutate(conf.low = estimate-1.96*std.error,conf.high = estimate+1.96*std.error)
  df
}


# Summary statistics
quicklook_column <- function(col) {
  if (is.integer(col)) { paste0(format(median(col,na.rm = TRUE),digits=3),
                                " (",format(quantile(col,1/4,na.rm=TRUE),digits=3),"-",
                                format(quantile(col,3/4,na.rm=TRUE),digits=3),")") }
  else if (is.numeric(col)) { paste0(format(mean(col,na.rm=TRUE),digits=3)," (",format(sd(col,na.rm=TRUE),digits=3),")") }
  else if(is.factor(col)) {
    if (length(levels(col))==2) {
      paste0(table(col) %>% .[[2]] %>% as.character," (",
             table(col) %>%prop.table %>% .[[2]] %>% format(digits=2) %>% as.character,")")
    } else {
      tab <- table(col) %>% prop.table
      ret <- ""
      for(i in seq_along(tab)) {
        ret <- paste0(ret,names(tab)[[i]],": ",format(tab[[i]],2),"; ")
      }
      return(ret)
    }
  } else { "Unknown"}
}

quicklook <- function(df) {
  bind_cols(
    df %>% summarize(N=n()),
    df %>% summarize_all(quicklook_column)
  )
}

library(stringr)
convert_monthyear_num <- function(x) {
  matches <- str_match(x,"([0-9]*)/([0-9]*)")
  as.numeric(matches[,3]) + as.numeric(matches[,2])/12
}

kable_loadings <- function (x, digits = 3L, cutoff = 0.1, sort = FALSE, ...) 
{
  Lambda <- unclass(x)
  p <- nrow(Lambda)
  factors <- ncol(Lambda)
  if (sort) {
    mx <- max.col(abs(Lambda))
    ind <- cbind(1L:p, mx)
    mx[abs(Lambda[ind]) < 0.5] <- factors + 1
    Lambda <- Lambda[order(mx, 1L:p), ]
  }
  fx <- setNames(format(round(Lambda, digits)), NULL)
  nc <- nchar(fx[1L], type = "c")
  fx[abs(Lambda) < cutoff] <- strrep(" ", nc)
  print(kable(fx, quote = FALSE, ...,caption="Loadings"))
  vx <- colSums(x^2)
  varex <- rbind(`SS loadings` = vx)
  if (is.null(attr(x, "covariance"))) {
    varex <- rbind(varex, `Proportion Var` = vx/p)
    if (factors > 1) 
      varex <- rbind(varex, `Cumulative Var` = cumsum(vx/p))
  }
  print(kable(round(varex, digits),caption="Variance Explained"))
  invisible(x)
}