library(DT)
library(dplyr)
library(htmltools)

createDataTable <- function(
  data_path, 
  dataset_url = NULL,  # Optional parameter to specify dataset URL, defaults to data_path
  displayed_columns = NULL, 
  currency_columns = NULL, 
  category_columns = NULL, 
  date_columns = NULL,  # New parameter for specifying date columns
  sort_columns = NULL, 
  sort_orders = NULL,
  column_widths = NULL,  # New parameter for column widths
  disable_sort_columns = NULL,
  style = "default",  # New parameter for table style (default or bootstrap)
  bootstrap_classes = NULL  # Additional Bootstrap classes for customization
) {
  # Read in the data
  data <- read.csv(data_path)
  
  # Extract the file name without the path or extension
  file_name <- tools::file_path_sans_ext(basename(data_path))
  
  # Set dataset_url to data_path if not provided
  if (is.null(dataset_url)) {
    dataset_url <- data_path
  }
  
  # Select all columns if displayed_columns is not specified
  if (is.null(displayed_columns)) {
    displayed_columns <- colnames(data)
  }
  
  # Select the desired columns
  data <- data %>% select(all_of(displayed_columns))
  
  # Convert specified columns to factors
  if (!is.null(category_columns)) {
    for (col in category_columns) {
      data[[col]] <- as.factor(data[[col]])
    }
  }
  
  # Convert specified date columns to Date format
  if (!is.null(date_columns)) {
    for (col in date_columns) {
      if (col %in% colnames(data)) {
        data[[col]] <- as.Date(data[[col]], format = "%Y-%m-%d") # Adjust the format if needed
      }
    }
  }
  
  # Set sorting options if specified
  order <- list()
  if (!is.null(sort_columns) && !is.null(sort_orders)) {
    for (i in seq_along(sort_columns)) {
      column_index <- which(displayed_columns == sort_columns[i]) - 1 # Zero-indexed for JS
      order[[i]] <- list(column_index, sort_orders[i])
    }
  }
  
  # Define column width settings if specified
  column_defs <- list()
  if (!is.null(column_widths)) {
    for (col_name in names(column_widths)) {
      col_index <- which(displayed_columns == col_name) - 1 # Zero-indexed for JS
      if (length(col_index) > 0) {
        column_defs <- append(column_defs, list(list(width = column_widths[[col_name]], targets = col_index)))
      }
    }
  }

  # Disable sorting on specific columns if specified
  if (!is.null(disable_sort_columns)) {
    disable_sort_indexes <- unlist(lapply(disable_sort_columns, function(col) {
      which(displayed_columns == col) - 1  # Convert to zero-indexed
    }))
    if (length(disable_sort_indexes) > 0) {
      column_defs <- append(column_defs, list(list(orderable = FALSE, targets = disable_sort_indexes)))
    }
  }
  
  # Determine table class based on the style parameter
  if (style == "bootstrap") {
    table_class <- DT:::DT2BSClass('display')
    if (!is.null(bootstrap_classes)) {
      # Add custom Bootstrap classes
      table_class <- paste(table_class, bootstrap_classes, collapse = " ")
    }
  } else {
    table_class <- "display"
  }

  # Render the DataTable with sorting, filename, display options, and column widths
  datatable(
    data,
    rownames = FALSE,
    class = table_class,  # Apply the selected table style with customizations
    filter = list(position = 'top'),
    extensions = c("Buttons", "FixedColumns", "Scroller"),
    options = list(
      autoWidth = TRUE,
      scrollX = TRUE,
      scrollY = 600,
       pageLength = 100,
    lengthMenu = c(10, 25, 50, 100, 1000),
      columnDefs = column_defs,
    #   deferRender = TRUE,
      dom = 'lfrtipB',
      buttons = list(
        list(extend = "copy"),
        list(
          extend = "csv",
          filename = file_name
        ),
        list(
          extend = "excel",
          filename = file_name
        ),
        # Add custom button to copy dataset URL
        list(
          extend = "collection",
          text = "<i class='fa fa-copy'></i> Copy Dataset URL",
          action = JS(
            sprintf(
              "function (e, dt, node, config) {
                var tempInput = document.createElement('input');
                tempInput.value = '%s';
                document.body.appendChild(tempInput);
                tempInput.select();
                document.execCommand('copy');
                document.body.removeChild(tempInput);
                alert('Dataset URL copied to clipboard: %s');
              }",
              dataset_url, dataset_url
            )
          )
        )
      ),
      order = order,
      initComplete = JS(
        "function(settings, json) {",
        "$('input[type=\"search\"]').attr('placeholder', 'Filter');",
        "}"
      )
    ),
    escape = FALSE
  ) %>% formatCurrency(currency_columns)
}
