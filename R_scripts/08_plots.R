
plot_elas = function(df,x, quadrant_limit,y_label,title,periods){
# builds a disperson plot of Rt vs median age/setting elasticities
# INPUTS: dataframe with columns Rt and median_cum_elas, name of the column to color by (x), limit to define quadrants, y axis label, title
# OUTPUT: ggplot object
  if(x == "setting"){
    pal <- c(
      "#984EA3",  # neutral grey - other
      "#E69F00",  # orange         - leisure
      "#56B4E9",  # light blue       - transport
      "#0072B2",  # strong blue   - school
      "#009E73",  # teal/green  - work
      "#D55E00"   # warm red-orange - home
    )}
  if(x == "age_group"){
      pal=  c(
         "#FFF2B2",   # light gold                       -[0,10)
         "#FFD772",  # brighter golden yellow           -[10,20)
         "#F9B74C",  # warm amber                       -[20,30)
         "#DFA144",  # muted orange-gold transitioning  -[30,40)
         "#A6B96A",  # olive–green transition           -[40,50)
         "#6FAF91",  # desaturated sea-green            -[50,60)
         "#4F9FAE",  # mid teal                         -[60,70)
         "#2B7F8E"   # deep teal                        -70+
      )
    }
  
  
  df <- df %>%
    mutate(
      quadrant = case_when(
        Rt >= 1 & median_cum_elas >= quadrant_limit ~ "Q1",
        Rt < 1  & median_cum_elas >= quadrant_limit ~ "Q2",
        Rt < 1  & median_cum_elas <  quadrant_limit  ~ "Q3",
        Rt >= 1 & median_cum_elas <  quadrant_limit  ~ "Q4"
      )
    )
  
  # hulls <- df %>%
  #   group_by(!!sym(x), quadrant) %>%
  #   slice(chull(Rt, median_cum_elas)) %>%
  #   ungroup()
  # 
  # centroids <- hulls %>%
  #   group_by(!!sym(x), quadrant) %>%
  #   summarise(
  #     cx = mean(Rt),
  #     cy = mean(median_cum_elas),
  #     .groups = "drop"
  #   )
  # 
  # arrow_df <- centroids %>%
  #   filter(quadrant %in% c("Q1", "Q2","Q4")) %>%
  #   pivot_wider(
  #     names_from = quadrant,
  #     values_from = c(cx, cy),
  #     names_sep = "_"
  #   ) %>%
  #   drop_na()
  # 
  # labels_hulls <- hulls %>%
  #   group_by(!!sym(x), quadrant) %>%
  #   summarise(
  #     Rt = mean(Rt),
  #     median_cum_elas = mean(median_cum_elas)
  #   ) %>%
  #   ungroup()
  # 

  
  
  p1=ggplot(df, aes(x = Rt, y = median_cum_elas, color = !!sym(x))) +
       #Points
       geom_point(size=1.5)+
       # geom_polygon(
       #   data = hulls,
       #   aes(fill = !!sym(x), group = interaction(!!sym(x), quadrant)),
       #   alpha = 0.3,
       #   color = NA
       # ) +
  # p1=ggplot(centroids,aes(x=cx,y=cy,color = !!sym(x)))+
  #     geom_point()+
    
      # Reference lines
      geom_hline(yintercept = quadrant_limit, linetype = "dashed", color = "grey50", linewidth = 0.8) +
      geom_vline(xintercept = 1, linetype = "dashed", color = "grey50", linewidth = 0.8) +
      geom_smooth(method="loess",se = FALSE, linewidth = 1,span = 0.5) +
      # arrows
     #  geom_segment(
     #    data = arrow_df,
     #    aes(
     #      x = cx_Q2, y = cy_Q2,
     #      xend = cx_Q1, yend = cy_Q1,
     #      color = age_group
     #    ),
     #    arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
     #    linewidth = 1
     #  ) +
     # geom_segment(
     #   data = arrow_df,
     #   aes(
     #     x = cx_Q1, y = cy_Q1,
     #     xend = cx_Q4, yend = cy_Q4,
     #     color = age_group
     #   ),
     #   arrow = arrow(length = unit(0.25, "cm"), type = "closed"),
     #   linewidth = 1
     # ) +
    
      # Labels
      # geom_text_repel(
      #   data = labels_hulls,
      #   aes(x = Rt, y = median_cum_elas, label = !!sym(x), color = !!sym(x)),
      #   size = 3.5,
      #   fontface = "bold",
      #   show.legend = FALSE
      # ) +
      
      # Axes labels
      labs(
        x = expression("\n Reproduction number " ~ (R[e])),
        y = y_label,
        color = "Age group",
        title = title,
        subtitle = paste0("period:",min(periods$start)," to ",max(periods$end))
      ) +
      
      # Colors: use Okabe–Ito palette (colorblind safe)
    scale_fill_manual(values = pal) +
    scale_color_manual(values = pal) +
      scale_y_continuous(
        breaks = scales::pretty_breaks(n = 10)
      ) +
      scale_x_continuous(
        breaks = scales::pretty_breaks(n = 10)
      ) +
      
      # Minimal but slightly more polished theme
      theme_minimal(base_size = 14) +
      theme(
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_line(linewidth = 0.3, color = "grey90"),
        panel.grid.major.y = element_line(linewidth = 0.3, color = "grey90"),
        plot.title = element_text(face = "bold", size = 16),
        plot.subtitle = element_text(size = 12, color = "grey30"),
        axis.title = element_text(face = "bold"),
        axis.text = element_text(color = "black"),
        legend.position = "bottom",
        legend.title = element_text(face = "bold"),
        legend.key.width = unit(1.2, "cm"),
        legend.spacing.x = unit(0.3, "cm")
      )
  
  periods_duration = periods$end-periods$start+1
  halfpoint = periods$start + periods_duration[1]/2
  
  p2=ggplot(df, aes(x = period, y = abs_contribution, fill = !!sym(x))) +
      # Stacked bars for age contributions
      geom_bar(stat = "identity", width = 0.9, alpha = 0.97, color = "white", linewidth = 0.2)+
  
      # Rt line and points
      geom_line(aes(y = Rt), color = "black", linewidth = 1.1) +
      geom_hline(yintercept = 1, linetype = "twodash", color = "black", linewidth = 0.7) +
      geom_point(aes(y = Rt ), color = "black", size = 3,show.legend = FALSE) +
      annotate("text", x = 5, y = 1.20, label = "R[e]", parse = TRUE, color = "black", fontface = "bold", size = 5) +
  
      # Y-axis scaling (primary = contributions, secondary = Rt)
      scale_y_continuous(
        name = expression("Absolute contributions to " * R[e]),
        breaks = seq(0.2,1.8,by=0.2),
        limits=c(0,1.8),
        expand=c(0,0)
      ) +
    
    scale_fill_manual(values = pal) +
  
      # Reference line for Rt = 1
  
      # Labels and title
      labs(
        x = "Period (half-point)",
        title = title,
        fill = "Age group"
      ) +
    theme_cowplot()+
    scale_x_continuous(
      breaks = seq(1,nrow(periods)),
      labels = halfpoint,
      expand = c(0.004, 0.004)
    ) +
    theme(panel.grid = element_blank())+
    theme(axis.text.x = element_text(angle = 90,vjust = 0.5,hjust = 1))+
    theme(
      legend.key.width = unit(1, "cm")
    )+
    theme(
      legend.position = "right",
      legend.title = element_text(size = 10),
      legend.text  = element_text(size = 8),
      legend.key.size = unit(0.5, "cm"),
      legend.spacing.x = unit(0.3, "cm"),
      legend.spacing.y = unit(0.1, "cm"),
      legend.margin = margin(0,0,0,0),
      legend.box.margin = margin(0,0,0,0)
    )+
    theme(
      axis.title.x = element_blank(),
      axis.text.x  = element_blank(),
      axis.ticks.x = element_blank()
    )
    
  return(list(p1,p2))
}
