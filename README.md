# Statistician

## What is this?

Statistician is a small program I created for Local Hack Day 2016. This was my
first time using the R programming language, and it was created to describe and
visualize datasets in a way that follows the AP Statistics guidelines. To do
this, it should ideally touch on four areas:

| Category         | Appropriate Statistics         |
| ---------------- | ------------------------------ |
| Center           | mean, median                   |
| Unusual Features | outliers, peaks, gaps          |
| Shape            | skewness, normality, symmetry  |
| Spread           | standard deviation, range, IQR |

## Installation

In order to detect skewness, Statistician relies on `e1071`, which can be
installed like so:

```r
install.package("e1071")
```

After that, you should be able to run Statistician from the command line and
follow its prompts without issue.

## License

Statistician is available under the MIT license. See the LICENSE file for details.
