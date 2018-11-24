require(ggplot2)
require(assertthat)
require(pdftools)
require(RColorBrewer)
require(fry9c)

repositoryPath <- file.path("C:","Users","Rob","Documents","Repositories")

fry_path <- file.path(repositoryPath, "fry9c", "doc")

if (FALSE)
{
  X1Q18 <- get_fry9c_data(2018, 1)
  X2Q18 <- get_fry9c_data(2018, 2)
  X4Q17 <- get_fry9c_data(2017, 4)
  X3Q17 <- get_fry9c_data(2017, 3)
  X2Q17 <- get_fry9c_data(2017, 2)
  X1Q17 <- get_fry9c_data(2017, 1)
  X1Q16 <- get_fry9c_data(2016, 1)
  X2Q16 <- get_fry9c_data(2016, 2)
  X3Q16 <- get_fry9c_data(2016, 3)
  X4Q16 <- get_fry9c_data(2016, 4)
  X1Q15 <- get_fry9c_data(2015, 1)
  X2Q15 <- get_fry9c_data(2015, 2)
  X3Q15 <- get_fry9c_data(2015, 3)
  X4Q15 <- get_fry9c_data(2015, 4)
  X1Q14 <- get_fry9c_data(2014, 1)
  X2Q14 <- get_fry9c_data(2014, 2, max_rows = 6000)
  X3Q14 <- get_fry9c_data(2014, 3)
  X4Q14 <- get_fry9c_data(2014, 4, max_rows = 6000)

  save(X1Q18, X2Q18,
       X4Q17, X3Q17, X2Q17, X1Q17,
       X4Q16, X3Q16, X2Q16, X1Q16,
       X4Q15, X3Q15, X2Q15, X1Q15,
       X4Q14, X3Q14, X2Q14, X1Q14,
       file = file.path(fry_path, "fry9data.Rdata"))
}
load(file.path(fry_path, "fry9data.Rdata"))

fry9c_data_list <- list(X1Q14, X2Q14, X3Q14, X4Q14,
                        X1Q15, X2Q15, X3Q15, X4Q15,
                        X1Q16, X2Q16, X3Q16, X4Q16,
                        X1Q17, X2Q17, X3Q17, X4Q17,
                        X1Q18, X2Q18)

rm(X1Q18, X2Q18,
   X4Q17, X3Q17, X2Q17, X1Q17,
   X4Q16, X3Q16, X2Q16, X1Q16,
   X4Q15, X3Q15, X2Q15, X1Q15,
   X4Q14, X3Q14, X2Q14, X1Q14)

bank_meta_data <- get_bank_meta_data()

################################################################################
# Bank Info #

targets <- c("HUNTINGTON BANCSHARES INCORPORATED",
             "KEYCORP",
             "PNC BANK CORP",
             "JPMORGAN CHASE",
             "FIFTH THIRD BANCORP",
             "U.S. BANCORP",
             "M&T BANK",
             "BB&T CORPORATION",
             "REGIONS FINANCIAL CORPORATION",
             "SUNTRUST BANKS, INC")

targets_short <- c("HBAN", "KEY", "PNC", "JPM", "FITB", "USB", "MTB", "BBT",
                   "RF", "STI")
assertthat::assert_that(length(targets) == length(targets_short))

pal <- brewer.pal(9, "Set1")
stock_cols <- c("HBAN" = "Green",
                "KEY" = pal[1],
                "PNC" = pal[2],
                "JPM" = pal[3],
                "FITB" = pal[4],
                "USB" = pal[5],
                "MTB" = pal[6],
                "BBT" = pal[7],
                "RF" = pal[8],
                "STI" = pal[9])
assertthat::assert_that(length(targets) == length(stock_cols))
stock_sizes <- c("HBAN" = 3,
                 "KEY" = 1,
                 "PNC" = 1,
                 "JPM" = 1,
                 "FITB" = 1,
                 "USB" = 1,
                 "MTB" = 1,
                 "BBT" = 1,
                 "RF" = 1,
                 "STI" = 1)
assertthat::assert_that(length(targets) == length(stock_sizes))

################################################################################

target_ids <- get_bank_ids(targets, bank_meta_data)

fry9cs <- Fry9c_group(years = c(rep(2014, 4), rep(2015, 4), rep(2016, 4), rep(2017, 4), 2018, 2018),
                      quarters = c(rep(1:4, times=4), 1:2))

fry9cs$parse_fry9c(file.path(repositoryPath, "fry9c", "inst", "extdata", c(
  "FR_Y-9C20140331.xml", "FR_Y-9C20140630.xml", "FR_Y-9C20140930.xml", "FR_Y-9C20141231.xml",
  "FR_Y-9C20150331.xml", "FR_Y-9C20150630.xml", "FR_Y-9C20150930.xml", "FR_Y-9C20151231.xml",
  "FR_Y-9C20160331.xml", "FR_Y-9C20160630.xml", "FR_Y-9C20160930.xml", "FR_Y-9C20161231.xml",
  "FR_Y-9C20170331.xml", "FR_Y-9C20170630.xml", "FR_Y-9C20170930.xml", "FR_Y-9C20171231.xml",
  "FR_Y-9C20180331.xml", "FR_Y-9C20180630.xml"
)))

fry9c_data_list_small <- lapply(fry9c_data_list, function(z) z[match(target_ids, z$RSSD9001),])

fry9cs$initializeData(fry9c_data_list_small, targets_short)

fry9cs$commonSize("HC-K", "BHCK3368", "HI")
fry9cs$commonSize("HC-K", "BHCK3368", "HI_Memo")

common_plots <- function(dat, nam)
{
  g1 <- ggplot(dat, aes(x = x, y = common_qdiff, group = bank, col = bank)) +
    geom_point(aes(size = bank)) +
    geom_line() +
    scale_y_continuous(name = paste0(nam, " / Average Assets"), labels = scales::percent) +
    xlab("Quarter") +
    scale_color_manual(values = stock_cols) +
    scale_size_manual(values = stock_sizes) +
    theme(legend.title = element_blank(),
          axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

  g2 <- ggplot(dat, aes(x = quarter, y = common_value, group = bank, col = bank)) +
    facet_grid(. ~ year) +
    geom_point(aes(size = bank)) +
    geom_line() +
    scale_y_continuous(name = paste0("YTD ", nam, " / Average Assets"), labels = scales::percent) +
    xlab("Quarter") +
    scale_color_manual(values = stock_cols) +
    scale_size_manual(values = stock_sizes) +
    theme(legend.title = element_blank())

  g3 <- ggplot(subset(dat, year > 2016),
               aes(x = quarter, y = common_yoy, group = bank, col = bank)) +
    facet_grid(. ~ year) +
    geom_point(aes(size = bank)) +
    geom_line() +
    scale_y_continuous(name = paste0("YOY Growth in ", nam, " / Average Assets"), labels = scales::percent) +
    xlab("Quarter") +
    scale_color_manual(values = stock_cols) +
    scale_size_manual(values = stock_sizes) +
    theme(legend.title = element_blank())

  plot(g1)
  plot(g2)
  plot(g3)
}

################################################################################

asset_data <- fry9cs$get_plot_data("HC-K", "5.")

ggplot(asset_data, aes(x = x, y = value, group = bank, col = bank)) +
  geom_point(aes(size = bank)) +
  geom_line() +
  scale_y_log10(name = "Total Assets (000s) (log scale)", labels = scales::dollar) +
  xlab("") +
  scale_color_manual(values = stock_cols) +
  scale_size_manual(values = stock_sizes) +
  theme(legend.title = element_blank(),
        axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

interest_income_data <- fry9cs$get_plot_data("HI", "1.h.")
common_plots(interest_income_data, "Interest Income")

interest_expense_data <- fry9cs$get_plot_data("HI", "2.f.")
common_plots(interest_expense_data, "Interest Expense")

noninterest_income_data <- fry9cs$get_plot_data("HI", "5.m.")
common_plots(interest_income_data, "Interest Income")

noninterest_expense_data <- fry9cs$get_plot_data("HI", "7.e.")
common_plots(noninterest_expense_data, "non-Interest Expense")

marketing_expense_data <- fry9cs$get_plot_data("HI_Memo", "7.b.")

ggplot(subset(marketing_expense_data, bank == "HBAN"), aes(x = x, y = qdiff, group = bank, col = bank)) +
  geom_point() +
  geom_line() + scale_y_continuous(name = "Marketing Expense (000s)", labels = scales::dollar) +
  xlab("Quarter") +
  scale_color_manual(values = stock_cols)

ggplot(subset(marketing_expense_data, bank == "HBAN"), aes(x = quarter, y = value, group = bank, col = bank)) +
  facet_grid(. ~ year) +
  geom_point() +
  geom_line() + scale_y_continuous(name = "YTD Marketing Expense (000s)", labels = scales::dollar) +
  xlab("Quarter") +
  scale_color_manual(values = stock_cols)

common_plots(marketing_expense_data, "Marketing Expense")

data_expense_data <- fry9cs$get_plot_data("HI_Memo", "7.a.")
common_plots(data_expense_data, "Data Expense")

employee_expense_data <- fry9cs$get_plot_data("HI", "7.a.")
common_plots(employee_expense_data, "Employee Expense")

fry9c_1Q18$exportExcel("text.xlsx")


