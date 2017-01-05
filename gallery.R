
library(survival)
library(ggplot2)
# library(beeswarm)
# library(ggbeeswarm)


# ----------------------------------------
# violin plot
# two categories

fan_plot_violin <- function(df, type='violin', strata=F){
    names(df) <- c('group', 'val')
    
    if(strata){
        df$group <- ifelse(df$group > median(df$group), 'high', 'low')
        df$group <- factor(df$group, levels=c('low', 'high'))
    }
        
    df$group <- factor(df$group)
    df <- subset(df, ! is.na(group))
    
    # wilcoxon rank sum test
    val.split = split(df$val, df$group)
    # t <- t.test(val.split[[1]], val.split[[2]])
    # p <- signif(t$p.value, 2)

    if(type == 'violin'){
        g <- ggplot(df, aes(group, val)) + 
            geom_violin(size=1, draw_quantiles = c(0.25, 0.5, 0.75), width=1)
    }else if(type == 'box'){
        g <- ggplot(df, aes(group, val)) + 
            geom_boxplot(outlier.shape=NA)
    }
    
    g <- g +
        geom_jitter(aes(color=group), width=0.1, size=2) +
        # geom_dotplot(aes(fill=group, color=group), binaxis='y', stackdir='center', dotsize=0.5) +
        # scale_y_log10() +
        theme_classic(base_size = 30) +
        theme(axis.line.x = element_line(colour = 'black', size=1),
              axis.line.y = element_line(colour = 'black', size=1),
              plot.title=element_text(face='bold')) +
        guides(color=F) +
        xlab('') + ylab('')
    
    if(strata){
        g <- g +
            scale_color_manual(breaks=c('low', 'high'), values=c('#00BFC4', '#F8766D'))
    }
    
    return(g)
}

# fan_plot_violin(iris[, c(5, 4)], 'box')
# fan_plot_violin(iris[, 1:2], strata=T)



# ------------------------------------------
# survial analysis and Kaplan-Meiyer curve

fan_plot_km <- function(df, legend.title=''){
    names(df) <- c('event', 'time', 'val')
    df$group <- ifelse(df$val > median(df$val), 'high', 'low')
    df$group <- factor(df$group, levels=c('low', 'high'))
    
    fit <- survfit(Surv(time, event) ~ group, data=df)
    res <- survdiff(data=df, Surv(time, event) ~ group)
    pval <- pchisq(res$chisq, length(res$n) - 1, lower.tail = F)
    pval <- signif(pval, 2)
    
#     p <- recordPlot()
#     plot.new()
    
    par(mar=c(5, 5, 2, 2))
    plot(fit, mark.time=T, col=c('#00BFC4', '#F8766D'), 
         xlab='Time', ylab='Survival Probability', cex.lab=1.5)
    legend(max(df$time) * 0.8, 1, col=c('#00BFC4', '#F8766D'), lty=1, bty='n',
           legend=c('low', 'high'), title=legend.title)
    text(max(df$time) * 0.8, 0.7, pos=4, labels=paste0('Log-rank test\np = ', pval))
}


# -----------------------------------------
# Gad Getz plot

fan_plot_getz <- function(df){
    names(df) <- c('factor.plot', 'value.plot')
    df.plot <- ddply(df, .(factor.plot), mutate, 
                     median.plot = median(value.plot),
                     # genius!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
                     id.plot = rank(value.plot) / length(value.plot))
    df.plot$factor.plot <- reorder(df.plot$factor.plot, df.plot$median.plot)
    df.plot$bg.plot <- ifelse(df.plot$factor.plot %in% levels(df.plot$factor.plot)[c(T, F)], 1, 2)
    
    g <- ggplot(df.plot) +
        # background
        geom_rect(aes(fill=factor(bg.plot)), alpha=0.3, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
        scale_fill_manual(breaks=c(1, 2), values=c('grey90', 'white')) +
        geom_point(aes(id.plot, value.plot), color='black') +
        # median segment
        geom_segment(aes(x=0.2, xend=0.8, y=median.plot, yend=median.plot), color='black') +
        facet_grid(. ~ factor.plot) +
        theme_classic(base_size=30) +
        theme(axis.ticks=element_blank(), axis.line=element_blank(), 
              axis.text.x=element_blank(), 
              legend.key.size=unit(2, 'lines'),
              # vjust is for making histology name closer to figure 
              strip.text.x=element_text(angle=90, vjust=0), 
              strip.background=element_blank(), 
              panel.spacing.x=unit(0, "lines")) +
        labs(x='', y='') +
        guides(fill=F)
    
    return(g)
}


# -----------------------------------------------------------------------








