widen <- function(data, by, keep){
  by <- enquo(by)

  static_cols <- data %>%
    gather(key, val, -c(CrossProject_ID, !!by)) %>%
    distinct() %>%
    group_by_at(vars(-val)) %>%
    add_tally %>%
    group_by(key) %>%
    summarise(m=max(n)) %>%
    filter(m==1) %>%
    select(key) %>%
    unlist() %>%
    c("CrossProject_ID",quo_name(by), .)

  #static_cols

  data2 <- data %>%
    gather(key, val, -one_of(static_cols)) %>%
    unite(key, c(!!by, key)) %>%
    distinct() %>%
    filter(!is.na(val))

  spread_data <- safely_spread(data2, key, val)

  if(is.null(spread_data$result)){

    rrr <- spread_data$error$message %>%
      as.character() %>%
      gsub("[[:alpha:]]|\\(|\\)| |", "", .) %>%
      str_split(",") %>%
      unlist() %>%
      as.numeric()

    print(data2 %>%
            slice(rrr))

    stop("There are duplicate entries. check the output above.")
  }
}

safely_spread <- safely(spread)

