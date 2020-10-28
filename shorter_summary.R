require(dplyr)
require(purrr)

deprive <- function (raw_score){
  raw_score <- raw_score * 4
  cats_score <- ordered(cut(raw_score ,breaks=c(-0.001,1.001,2.001,3.001,4.001),
                            labels=factor(c("Most deprived","Deprived","Somewhat deprived","Least deprived")),right = FALSE))
  return(cats_score)}


# Initialise easy summary function


# Initialise item score summary
summarise_item <- function(x, data = data, recode_string = NULL) {
  # Convert to factor
  if (class(pull(data[x])) == 'haven_labelled') {
    data[x] <- as_factor(pull(data[x]))
  }
  
  if(class(recode_string) == "character") {
    data$x_recode <- car::recode(pull(data[x]), recode_string)
  } else {
    data$x_recode <- pull(data[x])
  }
  
  females <- data %>% filter(Gender_ind == 'Female')
  males <- data %>% filter(Gender_ind == 'Male')
  
  graph_data <- data %>%
    select(x_recode, Gender_ind) %>%
    filter(Gender_ind =='Male' | Gender_ind == 'Female') %>%
    na.omit() %>%
    group_by(x_recode, Gender_ind) %>%
    count() %>%
    group_by(Gender_ind) %>%
    mutate(prop = prop.table(n))
  
  outplot <- ggplot(graph_data, aes(x = x_recode, y = prop, fill = Gender_ind)) +
    geom_col(position = 'dodge') +
    geom_text(size = 3, position = position_dodge(width = 1), aes(label = scales::percent(prop, 1), vjust = -1)) +
    labs(caption = paste0('Chi-squared p value = ', format(round(chisq.test(x = data$Gender_ind, y = data$x_recode)$p.value, 3), 
                                                           nsmall = 3),
                          '\nn = ', sum(!is.na(data$x_recode)), '\nn female = ', sum(!is.na(females$x_recode)), '\nn male = ', sum(!is.na(males$x_recode))))
  
  if (nrow(graph_data) > 6) {
    outplot + coord_flip()
  } else {
      outplot
    }
}

summarise_item_age <- function(x, data = data, recode_string = NULL) {
  # Convert to factor
  if (class(pull(data[x])) == 'haven_labelled') {
    data[x] <- as_factor(pull(data[x]))
  }
  
  if(class(recode_string) == "character") {
    data$x_recode <- car::recode(pull(data[x]), recode_string)
  } else {
    data$x_recode <- pull(data[x])
  }
  
  females <- data %>% filter(Age_ind == 'Female')
  males <- data %>% filter(Age_ind == 'Male')
  
  graph_data <- data %>%
    select(x_recode, Age_ind) %>%
    na.omit() %>%
    group_by(x_recode, Gender_ind) %>%
    count() %>%
    group_by(Gender_ind) %>%
    mutate(prop = prop.table(n))
  
  outplot <- ggplot(graph_data, aes(x = x_recode, y = prop, fill = Gender_ind)) +
    geom_col(position = 'dodge') +
    geom_text(size = 3, position = position_dodge(width = 1), aes(label = scales::percent(prop, 1), vjust = -1)) +
    labs(caption = paste0('Chi-squared p value = ', format(round(chisq.test(x = data$Gender_ind, y = data$x_recode)$p.value, 3), 
                                                           nsmall = 3),
                          '\nn = ', sum(!is.na(data$x_recode)), '\nn female = ', sum(!is.na(females$x_recode)), '\nn male = ', sum(!is.na(males$x_recode))))
  
  if (nrow(graph_data) > 6) {
    outplot + coord_flip()
  } else {
    outplot
  }
}





summarise_dim_scores <- function (score1, score2, df = data, factorise = TRUE, by_province = FALSE) {
  if (factorise) {
    df$fact_score1 <- deprive(pull(data[score1]))
  } else {
    df$fact_score1 <- pull(data[score1])
  }
  
  if (factorise) {
    df$fact_score2 <- deprive(pull(data[score2]))
  } else {
    df$fact_Score2 <- pull(data[score2])
  }
  
  if (!by_province) {
    df %>%
      select(fact_score1, Gender_ind, fact_score2) %>%
      filter(Gender_ind =='Male' | Gender_ind == 'Female') %>%
      na.omit() %>%
      group_by(fact_score1, fact_score2, Gender_ind) %>%
      count() %>%
      group_by(Gender_ind, fact_score2) %>%
      mutate(prop = prop.table(n)) %>%
      ggplot(aes(x = fact_score1, y = prop, fill = Gender_ind)) +
      geom_col(position = 'dodge') +
      scale_x_discrete(limits = rev(levels(data$fact_score))) +
      geom_text(size = 3, position = position_dodge(width = 1), aes(label = scales::percent(prop, 1), vjust = -1)) +
      facet_grid(cols = vars(fact_score2))
    
  } else if (by_province) {
    df %>%
      select(fact_score1, fact_score2, Gender_ind, Province_ind) %>%
      filter(Gender_ind =='Male' | Gender_ind == 'Female') %>%
      na.omit() %>%
      group_by(fact_score1, fact_score2, Gender_ind, Province_ind) %>%
      count() %>%
      group_by(Gender_ind, Province_ind, fact_score2) %>%
      mutate(prop = prop.table(n)) %>%
      ggplot(aes(x = fact_score1, y = prop, fill = Gender_ind)) +
      geom_col(position = 'dodge') +
      scale_x_discrete(limits = rev(levels(data$fact_score))) +
      geom_text(size = 3, position = position_dodge(width = 1), aes(label = scales::percent(prop, 1), vjust = -1)) +
      facet_grid(cols = vars(Province_ind), rows = vars(fact_score2))
  }
}



# Initialise item score summary
summarise_items <- function(x, y, recode_string1 = "", recode_string2 = "", by_province = FALSE) {
  if(class(recode_string1) == "character" & length(recode_string1) > 0) {
    data$x_recode <- car::recode(pull(data[,x]), recode_string1)
  } else {
    data$x_recode <- pull(data[,x])
  }
  
  if(class(recode_string2) == "character" & length(recode_string2) > 0) {
    data$y_recode <- car::recode(pull(data[,y]), recode_string2)
  } else {
    data$y_recode <- pull(data[,y])
  }
  
  if(!by_province) {
    data %>%
      select(x_recode, y_recode, Gender_ind) %>%
      filter(Gender_ind =='Male' | Gender_ind == 'Female') %>%
      na.omit() %>%
      group_by(x_recode, y_recode, Gender_ind) %>%
      count() %>%
      group_by(Gender_ind, y_recode) %>%
      mutate(prop = prop.table(n)) %>%
      ggplot(aes(x = x_recode, y = prop, fill = Gender_ind)) +
      geom_col(position = 'dodge') +
      geom_text(size = 3, position = position_dodge(width = 1), aes(label = scales::percent(prop, 1), vjust = -1)) +
      facet_grid(cols = vars(y_recode))
  } else if (by_province) {
    data %>%
      select(x_recode, y_recode, Gender_ind, Province_ind) %>%
      filter(Gender_ind =='Male' | Gender_ind == 'Female') %>%
      na.omit() %>%
      group_by(x_recode, y_recode, Gender_ind, Province_ind) %>%
      count() %>%
      group_by(Gender_ind, y_recode, Province_ind) %>%
      mutate(prop = prop.table(n)) %>%
      ggplot(aes(x = x_recode, y = prop, fill = Gender_ind)) +
      geom_col(position = 'dodge') +
      geom_text(size = 3, position = position_dodge(width = 1), aes(label = scales::percent(prop, 1), vjust = -1)) +
      facet_grid(cols = vars(Province_ind), rows = vars(y_recode))
  }
}




creat_rank_data <- function(data) {
  rank_data <- data %>%
    select(Ranking_1, Gender_ind, Age_bracket_ind, Province_ind, Disabled_ind) %>%
    mutate(split = str_split(.$'Ranking_1', '\\|')) %>%
    unnest(split)
  
  rank_data$split <- factor(rank_data$split, levels = paste0(1:15))
  return(rank_data)
}


make_dummy <- function (x, numeric_out = TRUE) {
  options <- x %>% as.character() %>% unique() %>% na.omit()
  out <- map(options, function (y) {y == x}) %>%
    bind_cols()
  names(out) <- options
  
  if (numeric_out) {
    out <- transmute_all(out, as.numeric)
  }
  
  return(out)
}

summarise_no_gender <- function(x, data = data, recode_string = NULL) {
  # Convert to factor
  if (class(pull(data[x])) == 'haven_labelled') {
    data[x] <- as_factor(pull(data[x]))
  }
  
  if(class(recode_string) == "character") {
    data$x_recode <- car::recode(pull(data[x]), recode_string)
  } else {
    data$x_recode <- pull(data[x])
  }
  
  data %>%
    select(x_recode) %>%
    na.omit() %>%
    group_by(x_recode) %>%
    count() %>%
    ungroup() %>%
    mutate(prop = prop.table(n)) %>%
    ggplot(aes(x = x_recode, y = prop)) +
    geom_col(position = 'dodge') +
    geom_text(size = 3, position = position_dodge(width = 1), aes(label = scales::percent(prop, 1), vjust = -1)) +
    labs(caption = '\nn = ', sum(!is.na(data$x_recode)))
}

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

summarise_multiple <- function(x, data = data, recode_string = NULL, titles = x) {
  if (length(x) == 1) {
    summarise_item(x = x, data = data, recode_string = recode_string)
  } else {
    
    for (i in 1:length(x)) {
      if (length(recode_string > 1)) {
        recode_string_i <- recode_string[i]
      } else {
        recode_string_i <- recode_string
      }
      
      cat('\n\n')
      print(summarise_item(x[i], data, recode_string = recode_string_i) + ggtitle(titles[i]))
      cat('\n\n')
    }
  }
}

summarise_multiple_age <- function(x, data = data, recode_string = NULL, titles = x) {
  if (length(x) == 1) {
    summarise_item(x = x, data = data, recode_string = recode_string)
  } else {
    
    for (i in 1:length(x)) {
      if (length(recode_string > 1)) {
        recode_string_i <- recode_string[i]
      } else {
        recode_string_i <- recode_string
      }
      
      cat('\n\n')
      print(summarise_item(x[i], data, recode_string = recode_string_i) + ggtitle(titles[i]))
      cat('\n\n')
    }
  }
}

# Cor matrix function

cor_matrix <- function (x) {
  cors <- x %>%
    map_df(as.numeric) %>%
    cor(use = "complete.obs") %>%
    round(2)
  
  melted_cormat <- melt(get_upper_tri(cors))
  
  ggplot(data = melted_cormat, aes(Var2, Var1, fill = value))+
    geom_tile(color = "white")+
    scale_fill_gradient2(low = "blue", high = "red", mid = "white", 
                         midpoint = 0, limit = c(-1,1), space = "Lab", 
                         name="Pearson\nCorrelation") +
    theme_minimal()+ 
    theme(axis.text.x = element_text(angle = 45, vjust = 1, 
                                     size = 12, hjust = 1))+
    coord_fixed() + 
    geom_text(aes(Var2, Var1, label = value), color = "black", size = 4) +
    theme(
      axis.title.x = element_blank(),
      axis.title.y = element_blank(),
      panel.grid.major = element_blank(),
      panel.border = element_blank(),
      panel.background = element_blank(),
      axis.ticks = element_blank(),
      legend.justification = c(1, 0),
      legend.position = c(0.6, 0.7),
      legend.direction = "horizontal")+
    guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                                 title.position = "top", title.hjust = 0.5))
}


deprive <- function (raw_score){
  raw_score <- raw_score * 4
  cats_score <- cut(raw_score ,breaks=c(-0.001,1.001,2.001,3.001,4.001),
                    labels=factor(c("Most deprived","Deprived","Somewhat deprived","Least deprived")), ordered = TRUE)
  return(cats_score)}

## norm_IDM
##
## This function is used to normalise scores to a 1-4 range
norm_IDM <- function(score, min, max) {
  # Check data
  if (max(score, na.rm = TRUE) > max) warning('Maximum value for score is higher than the maximum provided to function')
  if (max(score, na.rm = TRUE) < min) warning('Minimum value for score is lower than the minimum provided to function')
  
  rescale <- ((score-min)/(max - min))
  return(rescale)
}



y_summary <- function (x, y, data) {
  data['x_recode'] <- data[x]
  data['y_recode'] <- data[y]
  
  graph_data <- data %>%
    select(x_recode, y_recode) %>%
    na.omit() %>%
    group_by(x_recode, y_recode) %>%
    count() %>%
    group_by(y_recode) %>%
    mutate(prop = prop.table(n))
  
  
  outplot <- ggplot(graph_data, aes(x = x_recode, y = prop, fill = y_recode)) +
    geom_col(position = 'dodge') +
    geom_text(size = 3, position = position_dodge(width = 1), aes(label = scales::percent(prop, 1), vjust = -1)) +
    labs(caption = paste0(
                          '\nn = ', sum(!is.na(data$x_recode))))
  
  outplot
}

summarise_multiple_y <- function(x, y, data1, recode_string = NULL, titles = x) {
  if (length(x) == 1) {
    y_summary(x = x, y = y, data = data1, recode_string = recode_string)
  } else {
    
    for (i in 1:length(x)) {
      if (length(recode_string > 1)) {
        recode_string_i <- recode_string[i]
      } else {
        recode_string_i <- NULL
      }
      
      cat('\n\n')
      print(y_summary(x[i], y, data = data1) + ggtitle(titles[i]))
      cat('\n\n')
    }
  }
}


yz_summary <- function (x, y, z, data) {
  data['x_recode'] <- data[x]
  data['y_recode'] <- data[y]
  data['z_recode'] <- data[z]
  
  graph_data <- data %>%
    select(x_recode, y_recode, z_recode) %>%
    na.omit() %>%
    group_by(x_recode, y_recode, z_recode) %>%
    count() %>%
    group_by(y_recode, z_recode) %>%
    mutate(prop = prop.table(n))
  
  
  outplot <- ggplot(graph_data, aes(x = x_recode, y = prop, fill = y_recode)) +
    geom_col(position = 'dodge') +
    geom_text(size = 3, position = position_dodge(width = 1), aes(label = scales::percent(prop, 1), vjust = -1)) +
    labs(caption = paste0(
                          '\nn = ', sum(!is.na(data$x_recode)))) +
    facet_wrap(vars(z_recode))
  
    outplot <- outplot + coord_flip()
  
  outplot
}


summarise_multiple_yz <- function(x, y, z, data1, recode_string = NULL, titles = x) {
  if (length(x) == 1) {
    yz_summary(x = x, y = y, z = z, data = data1, recode_string = recode_string)
  } else {
    
    for (i in 1:length(x)) {
      if (length(recode_string > 1)) {
        recode_string_i <- recode_string[i]
      } else {
        recode_string_i <- NULL
      }
      
      cat('\n\n')
      print(yz_summary(x[i], y, z, data = data1) + ggtitle(titles[i]))
      cat('\n\n')
    }
  }
}

