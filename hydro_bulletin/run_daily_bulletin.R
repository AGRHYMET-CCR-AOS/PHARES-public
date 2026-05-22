# ============================================================
# Daily hydrological bulletin rendering
# ============================================================
time_ref <- format(Sys.Date(), "%Y%m%d")

# ============================================================
# Output file name
# ============================================================
output_file <- paste0(
  "Bulletin_hydrologique_",
  time_ref,
  ".docx"
)
pdf_output_file <- paste0(
  "Bulletin_hydrologique_",
  time_ref,
  ".pdf"
)

# ============================================================
# Render bulletin
# ============================================================
quarto::quarto_render(
  input = "bulletin_hydrologique.qmd",
  output_format = "docx",
  output_file = output_file,
  execute_dir = ".",
  execute_params = list(
    bulletin_number = paste0("BH-", time_ref),
    issue_date = format(Sys.Date(), "%d/%m/%Y"),
    validity_date = format(Sys.Date() + 10, "%d/%m/%Y")
  )
)

doconv::docx2pdf(
  input = output_file,
  output = pdf_output_file
)


# ============================================================
# Output directory
# ============================================================

daily_word_dir <- file.path(
  "outputs",
  "bulletins_word",
  time_ref
)

dir.create(
  daily_word_dir,
  recursive = TRUE,
  showWarnings = FALSE
)

# ============================================================
# Copy required rendering files
# ============================================================
file.copy(
  from =c(output_file, pdf_output_file),
  to = c(file.path(
    daily_word_dir,
    output_file
  ),
  file.path(
    daily_word_dir,
    pdf_output_file
  )),
  overwrite = TRUE
)
# ============================================================
# Clean temporary rendering directory
# ============================================================

file.remove(c(output_file, pdf_output_file))

message(
  "Bulletin generated successfully: ",
  file.path(getwd(),daily_word_dir,output_file)
)

