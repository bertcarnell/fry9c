% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/component.R
\name{component}
\alias{component}
\alias{.component}
\alias{Component}
\title{Class providing an object to manipulate a component of a schedule in a FR Y-9c}
\usage{
Component(num, name, key)
}
\arguments{
\item{num}{The line item number of a component of a schedule}

\item{name}{The name of the line item number}

\item{key}{The lookup key associated with the line item number}
}
\value{
an object of class \code{component}
}
\description{
Class providing an object to manipulate a component of a schedule in a FR Y-9c

Class providing an object to manipulate a component of a schedule in a FR Y-9c
}
\examples{
x <- Component("1.a.", "Income", "ZZZZ1234")
x$add(Component("1.a.(1)", "Sub-Income", "ABCD5555"))
x$initializeData(data.frame(ZZZZ1234 = 1:4,
                            ABCD5555 = 5:8))
x$export_csv()[1] == "1.a., Income, 1,2,3,4"
print(x)
all(x$getValueFromKey("ABCD5555") == 5:8)
all(x$getValueFromNum("1.a.") == 1:4)
all(x$getValue() == 1:4)
x$getKey() == "ZZZZ1234"
x$getNum() == "1.a."
x$getName() == "Income"
all(unlist(x$getAllValues()) == 1:8)
all(unlist(x$getAllNums()) == c("1.a.", "1.a.(1)"))
all(unlist(x$getAllNames() == c("Income", "Sub-Income")))
x$commonSize(100)
all.equal(x$getCommonSizeValueFromNum("1.a."), (1:4)/100)
all.equal(x$getCommonSizeValueFromKey("ZZZZ1234"), (1:4)/100)
all.equal(x$getCommonSizeValue(), (1:4)/100)
}
\section{Methods}{
\subsection{Public methods}{
\itemize{
\item \href{#method-new}{\code{.component$new()}}
\item \href{#method-initializeData}{\code{.component$initializeData()}}
\item \href{#method-add}{\code{.component$add()}}
\item \href{#method-export_csv}{\code{.component$export_csv()}}
\item \href{#method-print}{\code{.component$print()}}
\item \href{#method-getMetricFrom}{\code{.component$getMetricFrom()}}
\item \href{#method-getValueFromKey}{\code{.component$getValueFromKey()}}
\item \href{#method-getValueFromNum}{\code{.component$getValueFromNum()}}
\item \href{#method-getNumFromKey}{\code{.component$getNumFromKey()}}
\item \href{#method-getKeyFromNum}{\code{.component$getKeyFromNum()}}
\item \href{#method-getCommonSizeValueFromNum}{\code{.component$getCommonSizeValueFromNum()}}
\item \href{#method-getCommonSizeValueFromKey}{\code{.component$getCommonSizeValueFromKey()}}
\item \href{#method-getComponentFrom}{\code{.component$getComponentFrom()}}
\item \href{#method-getComponentFromKey}{\code{.component$getComponentFromKey()}}
\item \href{#method-getComponentFromNum}{\code{.component$getComponentFromNum()}}
\item \href{#method-getValue}{\code{.component$getValue()}}
\item \href{#method-getCommonSizeValue}{\code{.component$getCommonSizeValue()}}
\item \href{#method-getKey}{\code{.component$getKey()}}
\item \href{#method-getNum}{\code{.component$getNum()}}
\item \href{#method-getName}{\code{.component$getName()}}
\item \href{#method-getAllValues}{\code{.component$getAllValues()}}
\item \href{#method-getAllNums}{\code{.component$getAllNums()}}
\item \href{#method-getAllNames}{\code{.component$getAllNames()}}
\item \href{#method-commonSize}{\code{.component$commonSize()}}
\item \href{#method-clone}{\code{.component$clone()}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-new"></a>}}
\if{latex}{\out{\hypertarget{method-new}{}}}
\subsection{Method \code{new()}}{
initialize the values in each \code{component}
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{.component$new(num, name, key)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{num}}{The line item number of a component of a schedule}

\item{\code{name}}{The name of the line item number}

\item{\code{key}}{The lookup key associated with the line item number}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-initializeData"></a>}}
\if{latex}{\out{\hypertarget{method-initializeData}{}}}
\subsection{Method \code{initializeData()}}{
initialize object with data
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{.component$initializeData(dat)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{dat}}{A dataset from the Fed with all FR Y-9c data for a quarter.  The dataset contains columns with names that correspond to \code{key}s}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-add"></a>}}
\if{latex}{\out{\hypertarget{method-add}{}}}
\subsection{Method \code{add()}}{
add a sub-\code{component} to this component
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{.component$add(comp)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{comp}}{a component}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-export_csv"></a>}}
\if{latex}{\out{\hypertarget{method-export_csv}{}}}
\subsection{Method \code{export_csv()}}{
export this \code{component} in CSV format
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{.component$export_csv()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-print"></a>}}
\if{latex}{\out{\hypertarget{method-print}{}}}
\subsection{Method \code{print()}}{
print the \code{component} as a string
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{.component$print()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getMetricFrom"></a>}}
\if{latex}{\out{\hypertarget{method-getMetricFrom}{}}}
\subsection{Method \code{getMetricFrom()}}{
get a metric
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{.component$getMetricFrom(key, num = NA, metric = .metric$value)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{key}}{The lookup key associated with the line item number}

\item{\code{num}}{The line item number of a component of a schedule}

\item{\code{metric}}{the metric name}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getValueFromKey"></a>}}
\if{latex}{\out{\hypertarget{method-getValueFromKey}{}}}
\subsection{Method \code{getValueFromKey()}}{
get a \code{component} value from this object or a sub-\code{component} that matches the \code{key}
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{.component$getValueFromKey(key)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{key}}{The lookup key associated with the line item number}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getValueFromNum"></a>}}
\if{latex}{\out{\hypertarget{method-getValueFromNum}{}}}
\subsection{Method \code{getValueFromNum()}}{
get a \code{component} value from the \code{component} number in this object or a sub-\code{component}
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{.component$getValueFromNum(num)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{num}}{The line item number of a component of a schedule}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getNumFromKey"></a>}}
\if{latex}{\out{\hypertarget{method-getNumFromKey}{}}}
\subsection{Method \code{getNumFromKey()}}{
Get a num from a key
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{.component$getNumFromKey(key)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{key}}{The lookup key associated with the line item number}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getKeyFromNum"></a>}}
\if{latex}{\out{\hypertarget{method-getKeyFromNum}{}}}
\subsection{Method \code{getKeyFromNum()}}{
get a key from a num
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{.component$getKeyFromNum(num)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{num}}{The line item number of a component of a schedule}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getCommonSizeValueFromNum"></a>}}
\if{latex}{\out{\hypertarget{method-getCommonSizeValueFromNum}{}}}
\subsection{Method \code{getCommonSizeValueFromNum()}}{
get a \code{component} common-sized value from the \code{component} number in this object or a sub-\code{component}
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{.component$getCommonSizeValueFromNum(num)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{num}}{The line item number of a component of a schedule}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getCommonSizeValueFromKey"></a>}}
\if{latex}{\out{\hypertarget{method-getCommonSizeValueFromKey}{}}}
\subsection{Method \code{getCommonSizeValueFromKey()}}{
get a \code{component} common-sized value from the \code{component} \code{key} in this object or a sub-\code{component}
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{.component$getCommonSizeValueFromKey(key)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{key}}{The lookup key associated with the line item number}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getComponentFrom"></a>}}
\if{latex}{\out{\hypertarget{method-getComponentFrom}{}}}
\subsection{Method \code{getComponentFrom()}}{
get a Component
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{.component$getComponentFrom(key, num = NA)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{key}}{The lookup key associated with the line item number}

\item{\code{num}}{The line item number of a component of a schedule}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getComponentFromKey"></a>}}
\if{latex}{\out{\hypertarget{method-getComponentFromKey}{}}}
\subsection{Method \code{getComponentFromKey()}}{
Get a component from a key
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{.component$getComponentFromKey(key)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{key}}{The lookup key associated with the line item number}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getComponentFromNum"></a>}}
\if{latex}{\out{\hypertarget{method-getComponentFromNum}{}}}
\subsection{Method \code{getComponentFromNum()}}{
Get a component from a key
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{.component$getComponentFromNum(num)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{num}}{The line item number of a component of a schedule}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getValue"></a>}}
\if{latex}{\out{\hypertarget{method-getValue}{}}}
\subsection{Method \code{getValue()}}{
get the value of this object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{.component$getValue()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getCommonSizeValue"></a>}}
\if{latex}{\out{\hypertarget{method-getCommonSizeValue}{}}}
\subsection{Method \code{getCommonSizeValue()}}{
get the common-sized value of this object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{.component$getCommonSizeValue()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getKey"></a>}}
\if{latex}{\out{\hypertarget{method-getKey}{}}}
\subsection{Method \code{getKey()}}{
get the \code{key} from this object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{.component$getKey()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getNum"></a>}}
\if{latex}{\out{\hypertarget{method-getNum}{}}}
\subsection{Method \code{getNum()}}{
get the \code{num} from this object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{.component$getNum()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getName"></a>}}
\if{latex}{\out{\hypertarget{method-getName}{}}}
\subsection{Method \code{getName()}}{
get the \code{name} from this object
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{.component$getName()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getAllValues"></a>}}
\if{latex}{\out{\hypertarget{method-getAllValues}{}}}
\subsection{Method \code{getAllValues()}}{
get the values from this object and all sub-\code{component}s
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{.component$getAllValues()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getAllNums"></a>}}
\if{latex}{\out{\hypertarget{method-getAllNums}{}}}
\subsection{Method \code{getAllNums()}}{
get all the \code{num}s from this object and all sub-\code{component}s
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{.component$getAllNums()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-getAllNames"></a>}}
\if{latex}{\out{\hypertarget{method-getAllNames}{}}}
\subsection{Method \code{getAllNames()}}{
get all the \code{name}s from this object and all sub-\code{component}s
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{.component$getAllNames()}\if{html}{\out{</div>}}
}

}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-commonSize"></a>}}
\if{latex}{\out{\hypertarget{method-commonSize}{}}}
\subsection{Method \code{commonSize()}}{
common-size this component using the \code{divisor}
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{.component$commonSize(divisor)}\if{html}{\out{</div>}}
}

\subsection{Arguments}{
\if{html}{\out{<div class="arguments">}}
\describe{
\item{\code{divisor}}{a numeric that a \code{component}'s values are divided by}
}
\if{html}{\out{</div>}}
}
}
\if{html}{\out{<hr>}}
\if{html}{\out{<a id="method-clone"></a>}}
\if{latex}{\out{\hypertarget{method-clone}{}}}
\subsection{Method \code{clone()}}{
The objects of this class are cloneable with this method.
\subsection{Usage}{
\if{html}{\out{<div class="r">}}\preformatted{.component$clone(deep = FALSE)}\if{html}{\out{</div>}}
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
