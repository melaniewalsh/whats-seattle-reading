library(DT)
library(dplyr)
library(htmltools)

createDataTable <- function(
  data_path, 
  dataset_url = NULL,  
  displayed_columns = NULL, 
  currency_columns = NULL, 
  category_columns = NULL, 
  date_columns = NULL,  
  sort_columns = NULL, 
  sort_orders = NULL,
  column_widths = NULL,  
  disable_sort_columns = NULL,
  style = "default", 
  bootstrap_classes = NULL  
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
  
  # Convert category columns to factors
  if (!is.null(category_columns)) {
    for (col in category_columns) {
      data[[col]] <- as.factor(data[[col]])
    }
  }
  
  # Convert specified date columns to Date format
  if (!is.null(date_columns)) {
    for (col in date_columns) {
      if (col %in% colnames(data)) {
        data[[col]] <- as.Date(data[[col]], format = "%Y-%m-%d")
      }
    }
  }
  
  # Set sorting options if specified
  order <- list()
  if (!is.null(sort_columns) && !is.null(sort_orders)) {
    for (i in seq_along(sort_columns)) {
      column_index <- which(displayed_columns == sort_columns[i]) - 1
      order[[i]] <- list(column_index, sort_orders[i])
    }
  }
  
  # Define column width settings if specified
  column_defs <- list()
  if (!is.null(column_widths)) {
    for (col_name in names(column_widths)) {
      col_index <- which(displayed_columns == col_name) - 1
      if (length(col_index) > 0) {
        column_defs <- append(column_defs, list(list(width = column_widths[[col_name]], targets = col_index)))
      }
    }
  }

  # Disable sorting on specific columns if specified
  if (!is.null(disable_sort_columns)) {
    disable_sort_indexes <- unlist(lapply(disable_sort_columns, function(col) {
      which(displayed_columns == col) - 1
    }))
    if (length(disable_sort_indexes) > 0) {
      column_defs <- append(column_defs, list(list(orderable = FALSE, targets = disable_sort_indexes)))
    }
  }
  
  # Determine table class based on the style parameter
  if (style == "bootstrap") {
    table_class <- DT:::DT2BSClass('display')
    if (!is.null(bootstrap_classes)) {
      table_class <- paste(table_class, bootstrap_classes, collapse = " ")
    }
  } else {
    table_class <- "display"
  } 

  # Render the DataTable 
  datatable(
    data,
    rownames = FALSE,
    class = table_class,
    filter = list(position = 'top'),
    extensions = c("Buttons", "FixedColumns", "Scroller"),
    options = list(
      autoWidth = TRUE,
      scrollX = TRUE,
      scrollY = 600,
      pageLength = 100,
      lengthMenu = c(10, 25, 50, 100, 1000),
      columnDefs = column_defs,
      processing = TRUE,
      serverSide = FALSE,
      dom = 'lfrtipB',
      buttons = list(
        list(
          extend = "copy",
          text = "Copy"
        ),
        # CSV button remains unchanged
        list(
          extend = "csv",
          text = "CSV",
          action = JS(sprintf(
            "function ( e, dt, node, config ) {
              fetch('%s')
                .then(response => response.text())
                .then(data => {
                  var blob = new Blob([data], { type: 'text/csv' });
                  var url = window.URL.createObjectURL(blob);
                  var link = document.createElement('a');
                  link.href = url;
                  link.download = decodeURIComponent('%s.csv');
                  document.body.appendChild(link);
                  link.click();
                  window.URL.revokeObjectURL(url);
                  document.body.removeChild(link);
                });
            }", 
            dataset_url,
            file_name
          ))
        ),
        # Excel button with simpler CSV parsing
        list(
          extend = "excel",
          text = "Excel",
          action = JS(sprintf(
            "async function ( e, dt, node, config ) {
              // Helper function to escape XML special characters
              function escapeXml(unsafe) {
                if (typeof unsafe !== 'string') return '';
                return unsafe
                  .replace(/&/g, '&amp;')
                  .replace(/</g, '&lt;')
                  .replace(/>/g, '&gt;')
                  .replace(/\"/g, '&quot;')
                  .replace(/'/g, '&apos;');
              }

              try {
                // Fetch and parse CSV
                const response = await fetch('%s');
                const csvText = await response.text();
                
                // Split into rows
                const rows = csvText.split(/\\r?\\n/);
                const headers = rows[0].split(',').map(header => header.trim());
                const dataRows = rows.slice(1)
                  .filter(row => row.trim())
                  .map(row => row.split(',').map(cell => cell.trim()));

                // Create Excel parts
                const zip = new JSZip();
                
                // Add required Excel files
                zip.file('[Content_Types].xml',
                  '<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>' +
                  '<Types xmlns=\"http://schemas.openxmlformats.org/package/2006/content-types\">' +
                    '<Default Extension=\"xml\" ContentType=\"application/xml\"/>' +
                    '<Default Extension=\"rels\" ContentType=\"application/vnd.openxmlformats-package.relationships+xml\"/>' +
                    '<Override PartName=\"/xl/workbook.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.sheet.main+xml\"/>' +
                    '<Override PartName=\"/xl/worksheets/sheet1.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.worksheet+xml\"/>' +
                    '<Override PartName=\"/xl/styles.xml\" ContentType=\"application/vnd.openxmlformats-officedocument.spreadsheetml.styles+xml\"/>' +
                  '</Types>'
                );

                // Add .rels
                const rels = zip.folder('_rels');
                rels.file('.rels',
                  '<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>' +
                  '<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">' +
                    '<Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/officeDocument\" Target=\"xl/workbook.xml\"/>' +
                  '</Relationships>'
                );

                // Add xl folder
                const xl = zip.folder('xl');
                const xlRels = xl.folder('_rels');
                xlRels.file('workbook.xml.rels',
                  '<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>' +
                  '<Relationships xmlns=\"http://schemas.openxmlformats.org/package/2006/relationships\">' +
                    '<Relationship Id=\"rId1\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/worksheet\" Target=\"worksheets/sheet1.xml\"/>' +
                    '<Relationship Id=\"rId2\" Type=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships/styles\" Target=\"styles.xml\"/>' +
                  '</Relationships>'
                );

                // Add workbook.xml
                xl.file('workbook.xml',
                  '<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>' +
                  '<workbook xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\" xmlns:r=\"http://schemas.openxmlformats.org/officeDocument/2006/relationships\">' +
                    '<sheets><sheet name=\"Sheet1\" sheetId=\"1\" r:id=\"rId1\"/></sheets>' +
                  '</workbook>'
                );

                // Add styles.xml
                xl.file('styles.xml',
                  '<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>' +
                  '<styleSheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\">' +
                    '<fonts count=\"1\"><font><sz val=\"11\"/><name val=\"Calibri\"/></font></fonts>' +
                    '<fills count=\"1\"><fill><patternFill patternType=\"none\"/></fill></fills>' +
                    '<borders count=\"1\"><border><left/><right/><top/><bottom/><diagonal/></border></borders>' +
                    '<cellStyleXfs count=\"1\"><xf numFmtId=\"0\" fontId=\"0\" fillId=\"0\" borderId=\"0\"/></cellStyleXfs>' +
                    '<cellXfs count=\"1\"><xf numFmtId=\"0\" fontId=\"0\" fillId=\"0\" borderId=\"0\" xfId=\"0\"/></cellXfs>' +
                  '</styleSheet>'
                );

                // Create worksheet XML
                const worksheets = xl.folder('worksheets');
                let worksheet = '<?xml version=\"1.0\" encoding=\"UTF-8\" standalone=\"yes\"?>' +
                  '<worksheet xmlns=\"http://schemas.openxmlformats.org/spreadsheetml/2006/main\">' +
                  '<sheetData>';

                // Add headers
                worksheet += '<row>' + headers.map(h => 
                  `<c t=\"inlineStr\"><is><t>${escapeXml(h)}</t></is></c>`
                ).join('') + '</row>';

                // Add data rows
                dataRows.forEach(rowData => {
                  worksheet += '<row>' + rowData.map(cell => 
                    `<c t=\"inlineStr\"><is><t>${escapeXml(cell)}</t></is></c>`
                  ).join('') + '</row>';
                });

                worksheet += '</sheetData></worksheet>';
                worksheets.file('sheet1.xml', worksheet);

                // Generate and download
                const content = await zip.generateAsync({
                  type: 'blob',
                  mimeType: 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'
                });
                const url = window.URL.createObjectURL(content);
                const link = document.createElement('a');
                link.href = url;
                link.download = decodeURIComponent('%s.xlsx');
                document.body.appendChild(link);
                link.click();
                window.URL.revokeObjectURL(url);
                document.body.removeChild(link);
              } catch (error) {
                console.error('Error creating Excel file:', error);
                alert('Error creating Excel file. Please try again.');
              }
            }", 
            dataset_url,
            file_name
          ))
        ),
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