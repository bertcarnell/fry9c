% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fry9c_group.R
\name{fry9c_group}
\alias{fry9c_group}
\alias{.fry9c_group}
\alias{Fry9c_group}
\alias{length.fry9c_group}
\title{Class providing an object to manipulate a group of FR Y-9c templates, typically
used to manage FY Y-9c across years}
\usage{
Fry9c_group(years, quarters)

\method{length}{fry9c_group}(x, ...)
}
\arguments{
\item{years}{The years associated with the fry9c objects in the group}

\item{quarters}{The quarters associate with the fry9c objects in the group}

\item{x}{the \code{fry9c_group} object}

\item{...}{not used}
}
\value{
an object of class \code{fry9c_group}

the number of \code{fry9c} objects in the group
}
\description{
Class providing an object to manipulate a group of FR Y-9c templates, typically
used to manage FY Y-9c across years

Class providing an object to manipulate a group of FR Y-9c templates, typically
used to manage FY Y-9c across years
}
\examples{
# load example data
fry9c_data_list <- list(
  read.csv(system.file(file.path("extdata", "ex_BHCF1712.csv"), package = "fry9c")),
  read.csv(system.file(file.path("extdata", "ex_BHCF1812.csv"), package = "fry9c")))

my_fry9c_group <- Fry9c_group(years = c(2017, 2016),
                              quarters = c(4, 4))
my_fry9c_group$parse_fry9c(
  system.file(file.path("extdata", c("FR_Y-9C20171231.xml", "FR_Y-9C20161231.xml")),
              package = "fry9c"))

my_fry9c_group$initializeData(fry9c_data_list, paste("bank", LETTERS[1:10], sep=""))
print(my_fry9c_group)
length(my_fry9c_group) == 2

class(my_fry9c_group$get_fry9c(2016, 4))[1] == "fry9c"

my_fry9c_group$commonSize("HC-K", "BHCK3368", "HI") # 5.

nrow(my_fry9c_group$get_plot_data("HC-K", "BHCK3368")) == 20 # 5.
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{.fry9c_group$new()}}
\item \href{#method-parse_fry9c}{\code{.fry9c_group$parse_fry9c()}}
\item \href{#method-initializeData}{\code{.fry9c_group$initializeData()}}
\item \href{#method-get_fry9c}{\code{.fry9c_group$get_fry9c()}}
\item \href{#method-get_fry9c_list}{\code{.fry9c_group$get_fry9c_list()}}
\item \href{#method-commonSize}{\code{.fry9c_group$commonSize()}}
\item \href{#method-get_plot_data}{\code{.fry9c_group$get_plot_data()}}
\item \href{#method-length}{\code{.fry9c_group$length()}}
\item \href{#method-print}{\code{.fry9c_group$print()}}
\item \href{#method-clone}{\code{.fry9c_group$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
Initialize
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{.fry9c_group$new(years, quarters)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{years}}{The year associated with FR Y-9c in the list}

\item{\code{quarters}}{The quarter associated with each FR Y-9c in the list}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-parse_fry9c"></a>}}
\if{latex}{\out{\hypertarget{method-parse_fry9c}{}}}
\subsection{Method \code{parse_fry9c()}}{
Parse a collection of \code{files} which each represent a FR Y-9c schema.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{.fry9c_group$parse_fry9c(files)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{files}}{file names}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-initializeData"></a>}}
\if{latex}{\out{\hypertarget{method-initializeData}{}}}
\subsection{Method \code{initializeData()}}{
Parse a collection of FR Y-9c data files in \code{data_list}.  Also include a vector of \code{banks} that includes the names of the banks associated with the rows as they should appear in output.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{.fry9c_group$initializeData(data_list, banks)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{data_list}}{list of data objects}

\item{\code{banks}}{bank names}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_fry9c"></a>}}
\if{latex}{\out{\hypertarget{method-get_fry9c}{}}}
\subsection{Method \code{get_fry9c()}}{
Extract a \code{fry9c} object from the collection associated with a \code{year} and \code{quarter}
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{.fry9c_group$get_fry9c(year, quarter)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{year}}{The year associated with FR Y-9c in the list}

\item{\code{quarter}}{The quarter associated with each FR Y-9c in the list}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_fry9c_list"></a>}}
\if{latex}{\out{\hypertarget{method-get_fry9c_list}{}}}
\subsection{Method \code{get_fry9c_list()}}{
Get a FRY-9C list
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{.fry9c_group$get_fry9c_list(years, quarters)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{years}}{The year associated with FR Y-9c in the list}

\item{\code{quarters}}{The quarter associated with each FR Y-9c in the list}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-commonSize"></a>}}
\if{latex}{\out{\hypertarget{method-commonSize}{}}}
\subsection{Method \code{commonSize()}}{
Common side the \code{sched} using the element from the \code{divisor_sched} and \code{divisor_key}.  This is usually done by common sizing an income statement with the average assets.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{.fry9c_group$commonSize(divisor_sched, divisor_key, sched)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{divisor_sched}}{the schedule of the divisor}

\item{\code{divisor_key}}{the key of the divisor}

\item{\code{sched}}{the schedule}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-get_plot_data"></a>}}
\if{latex}{\out{\hypertarget{method-get_plot_data}{}}}
\subsection{Method \code{get_plot_data()}}{
Create a \code{data.frame} that can be used for plotting using \code{ggplot2} by selecting a schedule \code{sched} and element number \code{key}
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{.fry9c_group$get_plot_data(sched, key, num = NA)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{sched}}{the schedule}

\item{\code{key}}{the key in the schedule}

\item{\code{num}}{the schedule element}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-length"></a>}}
\if{latex}{\out{\hypertarget{method-length}{}}}
\subsection{Method \code{length()}}{
Return the number of fry9c objects in the collection
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{.fry9c_group$length()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-print"></a>}}
\if{latex}{\out{\hypertarget{method-print}{}}}
\subsection{Method \code{print()}}{
Print a summary of the collection contents
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{.fry9c_group$print()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{.fry9c_group$clone(deep = FALSE)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{deep}}{Whether to make a deep clone.}
}
\if{html}{\out{</div>}}
}
}
}
