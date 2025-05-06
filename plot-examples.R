library(dplyr)
library(ggplot2)
library(scico)
library(tidyr)
library(lubridate)

### === Utilities =================================================================================

.money_format_single <- function(x) {
    if (is.na(x)) NA
    else if (x == 0) "$0"
    else if (x < 1) sprintf("$%.2f", x)
    else if (x < 1000) sprintf("$%d", round(x))
    else if (x < 1e6) sprintf("$%dK", round(x / 1e3))
    else if (x < 1e9) sprintf("$%dM", round(x / 1e6))
    else if (x < 1e12) sprintf("$%dB", round(x / 1e9))
    else if (x < 1e15) sprintf("$%dT", round(x / 1e12))
    else sprintf("$$$%f", x)
}

money_format <- function(x) sapply(x, .money_format_single)

theme_spring <- function(base_size=11, label_scale=1.0, font="Trebuchet MS") {
    # Theme for the Spring CAS Meeting
    # You might have trouble with the font I'm using by default if you haven't manually
    # installed extra fonts in R. You can swap it out for font="sans" and get nearly the same
    # results.
    theme_minimal() +
        theme(axis.title.x = element_text(family=font, margin=margin(), size=base_size),
              axis.title.y = element_text(family=font, margin=margin(), size=base_size),
              legend.title = element_text(family=font, margin=margin(), size=base_size),
              legend.text = element_text(family=font, margin=margin(), size=0.85*base_size),
              axis.text = element_text(family=font, margin=margin(), size=0.85*base_size*label_scale),
              strip.background = element_rect(fill="grey97", color="grey97"),
              strip.text = element_text(family=font, margin=margin(t=1, b=1), size=0.9*base_size),
              panel.spacing.x = unit(5, "points"),
              panel.spacing.y = unit(3, "points"),
              legend.margin = margin(),
              plot.background = element_rect(fill="white", color="white"))
}

.add_final_indicator <- function(tri.df, group.vars=c()) {
    # Add a boolean variable that TRUE iff it's the last observed dev lag for a given
    # accident year
    tri.df |>
        group_by_at(c("period_start", group.vars)) |>
        arrange(desc(dev_lag)) |>
        mutate(is_final = 1:n() == 1) |>
        ungroup()
}

.add_incrementals <- function(tri.df, group.vars=c()) {
    # Add derived incremental variables
    tri.df |>
        group_by_at(c("period_start", group.vars)) |>
        arrange(dev_lag) |>
        mutate(incr_paid_loss = diff(c(0, paid_loss)),
               incr_reported_loss = diff(c(0, reported_loss)),
               incr_incurred_loss = diff(c(0, incurred_loss)),
               incr_reported_claims = diff(c(0, reported_claims)),
               incr_open_claims = diff(c(0, open_claims))) |>
        ungroup()
}

### === Plots! ====================================================================================

# All of these plots are reference implementations of single-triangle versions of the plot.
# Any of these plots can be faceted.
# 
# By default, the plots show development lags in years from the end of the experience period.
# If your convention is to show development lags as months from the start of the experience period,
# go ahead -- the changes should be fairly straightforward.
# 
# Some plots have specific additional configuration options that should be made available,
# which are called out in comments in the body of each plot function.

plot_heatmap <- function(tri.df, loss_definition = "paid") {
    # Should extend to work on losses and claim counts, not just LRs
    # Obvious changes to labels (currency / count formats instead of percents) when not LRs
    tri.df$loss_ratio <- tri.df[[paste0(loss_definition, "_loss")]] / tri.df$earned_premium
    
    ggplot(tri.df) +
        aes(y=period_start, x=dev_lag / 12, fill=paid_loss / earned_premium,
            label=sprintf("%.1f%%", paid_loss / earned_premium * 100)) +
        geom_raster() +
        geom_text(color="white") +
        labs(y="Accident Period", x="Development Lag (Years)", fill="Paid LR") +
        scale_x_continuous(breaks=seq(0, 8, 2)) +
        scale_y_continuous(trans=c("date", "reverse")) +
        scale_fill_scico(palette="managua", labels=scales::percent) +
        theme_spring()
}

plot_growth_curve <- function(tri.df, loss_definition = "paid") {
    # Should extend to work on losses and claim counts, not just LRs
    # Other derived metrics are possible too: 
    #       paid/reported ratio, mean open reserve, mean reported severity, etc
    # Obvious changes to labels (currency / count formats instead of percents) when not LRs
    
    tri.df$loss_ratio <- tri.df[[paste0(loss_definition, "_loss")]] / tri.df$earned_premium
    rich.tri.df <- .add_final_indicator(tri.df)
    
    ggplot(rich.tri.df) +
        aes(x=dev_lag / 12, y=paid_loss / earned_premium, color=period_start,
            group=strftime(period_start, '%Y')) +
        geom_line(alpha=0.2) +
        geom_point(aes(alpha=ifelse(is_final, 1.0, 0.2), size=ifelse(is_final, 2.5, 1))) +
        labs(y="Loss Ratio", x="Development Lag (Years)", color="Accident\nPeriod") +
        scale_x_continuous(breaks=seq(0, 8, 2)) +
        scale_y_continuous(labels=scales::percent) +
        scale_alpha_identity() +
        scale_size_identity() +
        scale_color_scico(palette="managua") +
        theme_spring()
}

plot_mountain <- function(tri.df, loss_definition = "paid") {
    # Should extend to work on losses and claim counts, not just LRs
    # Other derived metrics are possible too:
    #       paid/reported ratio, mean open reserve, mean reported severity, etc
    # Obvious changes to labels (currency / count formats instead of percents) when not LRs
    
    tri.df$loss_ratio <- tri.df[[paste0(loss_definition, "_loss")]] / tri.df$earned_premium
    rich.tri.df <- .add_final_indicator(tri.df)
    
    ggplot(rich.tri.df) +
        aes(x=period_start, y=paid_loss / earned_premium, color=dev_lag / 12, group=dev_lag / 12) +
        geom_line(alpha=0.6) +
        geom_point(aes(alpha=ifelse(is_final, 1.0, 0.2), size=ifelse(is_final, 2, 1))) +
        labs(y="Paid Loss Ratio", x="Accident Period", color="Dev Lag") +
        scale_x_continuous() +
        scale_y_continuous(labels=scales::percent) +
        scale_alpha_identity() +
        scale_size_identity() +
        scale_color_scico(palette="managua", breaks=seq(0, 8, 2)) +
        theme_spring()
}

plot_claim_closure <- function(tri.df) {
    rich.tri.df <- .add_final_indicator(tri.df) |>
        mutate(dev_lag_years = dev_lag / 12)
    
    # This entire blocks is just for the sake of the label with the annual factor.
    linreg <- lm(log(open_claims / reported_claims) ~ dev_lag_years, data=rich.tri.df)
    df.summary <- rich.tri.df |>
        filter(open_claims > 0) |>
        summarize(xmin = min(dev_lag / 12),
                  xmax = max(dev_lag / 12),
                  ymin = min(log10(open_claims / reported_claims)),
                  ymax = max(log10(open_claims / reported_claims)))
    ypos <- 0.15 * df.summary$ymin + 0.85 * df.summary$ymax
    xpos <- 0.3 * df.summary$xmin + 0.7 * df.summary$xmax
    
    ggplot(rich.tri.df) +
        aes(x=dev_lag / 12, y=open_claims / reported_claims, color=period_start,
            group=period_start) +
        geom_smooth(aes(group=NULL), method="lm", linewidth=0.6, color="forestgreen", fill="forestgreen", alpha=0.3) +
        geom_line(alpha=0.3) +
        geom_label(x=xpos, y=ypos, size=4.5, label=sprintf("Decay Factor: %.3f", exp(coef(linreg)[2])), color="black") +
        geom_point(aes(alpha=ifelse(is_final, 1.0, 0.2), size=ifelse(is_final, 2.5, 1))) +
        labs(y="Open Claim Share", x="Development Lag (Years)", color="Accident\nYear") +
        scale_x_continuous(breaks=seq(0, 8, 2)) +
        scale_y_log10(labels=scales::percent) +
        scale_alpha_identity() +
        scale_size_identity() +
        scale_color_scico(palette="managua", 
                          labels=function(.) strftime(as.Date(., origin=as.Date("1970-01-01")), "%Y")) +
        theme_spring()
}

plot_sunset <- function(tri.df, metric = "paid_loss") {
    # Should extend to work on claim count development, not just loss development
    ata.df <- .add_incrementals(tri.df)
    incr.vals <- ata.df[[paste0("incr_", metric)]]
    vals <- ata.df[[metric]]
    ata.df$ata <- pmax(incr.vals / (vals - incr.vals), 1e-6)
    
    ggplot(ata.df |> filter(dev_lag > 0)) +
        aes(x=period_start %m+% months(dev_lag), y=ata,
            group=dev_lag / 12, color=dev_lag / 12, fill=dev_lag / 12) +
        geom_smooth(alpha=0.0, linewidth=0.6) +
        geom_point(alpha=0.6) +
        scale_color_scico(palette="managua", breaks=seq(0, 8, 2)) +
        scale_fill_scico(palette="managua", breaks=seq(0, 8, 2)) +
        scale_y_continuous(transform=scales::transform_boxcox(p=0.3),
                           limits = c(0, 1.6),
                           breaks=c(0, 0.01, 0.05, 0.1, 0.2, 0.5, 1.0, 1.5)) +
        labs(x="Calendar Period", y="Incremental ATA Factor", color="Dev Lag", fill="Dev Lag") +
        theme_spring()
}

plot_ballistic <- function(tri.df) {
    # Should extend to work on losses, not just loss ratios.
    # Obvious changes to labels (currency / count formats instead of percents) when not LRs
    rich.tri.df <- .add_final_indicator(tri.df)
    
    ggplot(rich.tri.df) +
        aes(x=paid_loss / earned_premium, y=reported_loss / earned_premium,
            color=dev_lag / 12, group=period_start) +
        geom_path(alpha=0.6) +
        geom_point(aes(alpha=ifelse(is_final, 1.0, 0.2), size=ifelse(is_final, 2, 1))) +
        geom_abline(slope=1, intercept=0, color="purple1") +
        labs(y="Reported Loss Ratio", x="Paid Loss Ratio", color="Dev Lag") +
        scale_x_continuous(labels=scales::percent) +
        scale_y_continuous(labels=scales::percent) +
        scale_alpha_identity() +
        scale_size_identity() +
        scale_color_scico(palette="managua", breaks=seq(0, 8, 2)) +
        theme_spring()
}

plot_broom <- function(tri.df) {
    # Should extend to work on losses, not just loss ratios.
    # Obvious changes to labels (currency / count formats instead of percents) when not LRs
    rich.tri.df <- .add_final_indicator(tri.df)
    
    ggplot(rich.tri.df) +
        aes(y=paid_loss / earned_premium, x=paid_loss / reported_loss,
            color=dev_lag / 12, group=period_start) +
        geom_path(alpha=0.6) +
        geom_point(aes(alpha=ifelse(is_final, 1.0, 0.2), size=ifelse(is_final, 2, 1))) +
        labs(x="Paid / Reported Ratio", y="Paid Loss Ratio", color="Dev Lag") +
        scale_x_continuous(labels=scales::percent) +
        scale_y_continuous(labels=scales::percent) +
        scale_alpha_identity() +
        scale_size_identity() +
        scale_color_scico(palette="managua", breaks=seq(0, 8, 2)) +
        theme_spring()
}

plot_hose <- function(tri.df, loss_definition = "paid") {
    # Should extend to work on claim counts and losses, not just loss ratios.
    # Obvious changes to labels (currency / count formats instead of percents) when not LRs
    incr.tri.df <- tri.df |>
        .add_final_indicator() |>
        .add_incrementals()
    
    incr.tri.df$incr_lr <- incr.tri.df[[paste0("incr_", loss_definition, "_loss")]] / incr.tri.df$earned_premium
    incr.tri.df$lr <- incr.tri.df[[paste0(loss_definition, "_loss")]] / incr.tri.df$earned_premium
    
    ggplot(incr.tri.df) +
        aes(y=incr_lr, x=lr, color=dev_lag / 12, group=period_start) +
        geom_path(alpha=0.6) +
        geom_point(aes(alpha=ifelse(is_final, 1.0, 0.2), size=ifelse(is_final, 2, 1))) +
        labs(y="Incremental Paid Loss Ratio", x="Paid Loss Ratio", color="Dev Lag") +
        scale_x_continuous(labels=scales::percent) +
        scale_y_continuous(labels=scales::percent) +
        scale_alpha_identity() +
        scale_size_identity() +
        scale_color_scico(palette="managua", breaks=seq(0, 8, 2)) +
        theme_spring()
}

plot_drip <- function(tri.df) {
    # Should extend to work with paid LR and paid/reported loss.
    rich.tri.df <- .add_final_indicator(tri.df)
    
    ggplot(rich.tri.df) +
        aes(x=reported_loss / earned_premium, y=open_claims / reported_claims,
            color=dev_lag / 12, group=period_start) +
        geom_path(alpha=0.6) +
        geom_point(aes(alpha=ifelse(is_final, 1.0, 0.2), size=ifelse(is_final, 2, 1))) +
        labs(x="Reported Loss Ratio", y="Closed Claims Ratio", color="Dev Lag") +
        scale_x_continuous(labels=scales::percent) +
        scale_y_log10(labels=scales::percent) +
        scale_alpha_identity() +
        scale_size_identity() +
        scale_color_scico(palette="managua", breaks=seq(0, 8, 2)) +
        theme_spring()
}

plot_state_outlier <- function(tri.df, dev.lag = 12) {
    # There's a biiiig configuration rabbit hole we can go down here.
    # Accept function-typed args to define custom metrics on the x- and y- axis?
    rich.tri.df <- .add_final_indicator(tri.df) |>
        .add_incrementals() |>
        mutate(ep_weight = earned_premium / max(earned_premium))
    
    ggplot(rich.tri.df |> filter(dev_lag == dev.lag)) +
        aes(x=paid_loss / reported_loss, y=paid_loss / earned_premium,
            alpha=ep_weight,
            color=ifelse(is_final, "orange", "steelblue"),
            size=ifelse(is_final, 3, 1.5)) +
        geom_segment(aes(size=NULL,
                         xend=paid_loss / reported_loss + 
                         0.05 * ((paid_loss - incr_paid_loss) / (reported_loss - incr_reported_loss) 
                                - (paid_loss / reported_loss)),
                     yend=paid_loss / earned_premium +
                        0.05 * ((paid_loss - incr_paid_loss) / earned_premium - paid_loss / earned_premium)),
                     linewidth=1) +
        geom_point() +
        scale_size_identity() +
        scale_color_identity() +
        scale_alpha_identity() +
        scale_x_continuous(labels=scales::percent) +
        scale_y_continuous(labels=scales::percent) +
        labs(y="Paid Loss Ratio", x="Paid / Reported Ratio", alpha="Premium", color="Alias") +
        theme_spring()
}

tri.df <- readRDS("example_tri.Rds")
plot_heatmap(tri.df)
plot_growth_curve(tri.df)
plot_mountain(tri.df)
plot_claim_closure(tri.df)
plot_sunset(tri.df)
plot_ballistic(tri.df)
plot_broom(tri.df)
plot_hose(tri.df)
plot_drip(tri.df)
plot_state_outlier(tri.df)
