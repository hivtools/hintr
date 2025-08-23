read_har <- function(path, source_site) {
  json <- jsonlite::read_json(path)
  entries <- lapply(json$log$entries, parse_entry)
  entries <- discard_non_hint(entries)
  entries <- lapply(entries, function(entry) {
    entry$source <- source_site
    entry
  })
  entries <- group_entries(entries)
}

## Parse a har file entry, contains keys
## startedDateTime
## request
## response
## We want the time time, url
##
parse_entry <- function(entry) {
  list(
    url = entry$request$url,
    start = entry$startedDateTime,
    time = entry$time
  )
}

hint_domains <- list(
  "naomi.unaids.org",
  "naomi-preview.dide.ic.ac.uk",
  "nm-hint.azurewebsites.net"
)

is_hint_entry <- function(entry) {
  urltools::domain(entry$url) %in% hint_domains
}

discard_non_hint <- function(entries) {
  hint_entry <- vapply(entries, is_hint_entry, logical(1))
  entries[hint_entry]
}

group_entries <- function(entries) {
  add_group <- function(entry) {
    ## Pattern to label I want to use
    entrypoint <- urltools::path(entry$url)
    groups <- list(
      "adr/datasets/<id>" = "adr\\/datasets\\/",
      "project/<id>/version" = "project\\/\\d+\\/version",
      "<type>/status/<id>" = "\\w+\\/status\\/\\w+",
      "meta/adr/<id>" = "meta\\/adr\\/",
      "meta/review-inputs/<iso3>" = "meta\\/review-inputs\\/",
      "model/result/<id>" = "model\\/result\\/",
      "calibrate/options/<iso3>" = "calibrate\\/options\\/",
      "calibrate/submit/<id>" = "calibrate\\/submit\\/",
      "calibrate/result/metadata/<id>" = "calibrate\\/result\\/metadata\\/",
      "calibrate/result/filteredData/<id>" = "calibrate\\/result\\/filteredData\\/",
      "calibrate/plot/<id>" = "calibrate\\/plot\\/",
      "model/comparison/plot/<id>" = "model\\/comparison\\/plot\\/",
      "download/submit/<type>/<id>" = "download\\/submit\\/",
      "download/result/<id>" = "download\\/result\\/"
    )
    for (group in names(groups)) {
      if (grepl(groups[[group]], entrypoint)) {
        entrypoint <- group
      }
    }
    entry$group <- entrypoint
    entry
  }
  do.call(rbind.data.frame, lapply(entries, add_group))
}

summarise_har <- function(har) {
  har |>
    dplyr::group_by(group, source) |>
    dplyr::summarize(mean = mean(time), sd = sd(time), n = dplyr::n()) |>
    dplyr::ungroup()
}

plot_har <- function(har, n_bars) {
  har <- dplyr::filter(har, mean > 0)
  ggplot2::ggplot(har, ggplot2::aes(x = group, y = mean, fill = source)) +
    ggplot2::geom_bar(position = ggplot2::position_dodge(), stat = "identity") +
    ggplot2::geom_errorbar(ggplot2::aes(ymin = mean - sd, ymax = mean + sd),
                           size = 0.3,
                           width = 0.2,
                           position = ggplot2::position_dodge(0.9)) +
    ggplot2::xlab("Endpoint") +
    ggplot2::ylab("Mean time") +
    ggplot2::theme_minimal() +
    ggplot2::ggtitle("Mean request time") +
    ggplot2::theme(axis.text.x = ggplot2::element_text(angle = 45, hjust = 1))
}

plot_all <- function(paths, n_bars) {
  har <- lapply(names(paths), function(nm) read_har(paths[[nm]], nm))
  har <- do.call(rbind.data.frame, har)
  summary <- summarise_har(har)
  plot_har(summary, n_bars)
}

## Update this to point to your files
paths <- list(
  "UK Azure" = "~/nm-hint.azurewebsites.net_Archive [25-02-24 17-28-55].har",
  "UK Prod" = "~/naomi.unaids.org_Archive [25-02-24 17-27-17].har",
  "US Azure" = "~/Downloads/nm-hint.azurewebsites.net.har",
  "US Prod" = "~/Downloads/naomi.unaids.org.har")
plot_all(paths, 10)
