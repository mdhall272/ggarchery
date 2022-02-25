# ggarchery: Flexible segment geoms with arrows for ggplot2


ggarchery is intended to extend [ggplot2](https://github.com/tidyverse/ggplot2)'s handling of segments with arrowheads. At present it contains one geom and one position adjustment.

## `geom_arrowsegment()` allows placement of one or more arrowheads at any point on a segment

First, let's generate some data that would be understood by [ggplot2](https://github.com/tidyverse/ggplot2)'s normal `geom_segment()`:

```
library(tidyverse)
library(ggarchery)

tbl <- tibble(x = c(0.1, 0.2), xend = c(0.1, 0.8), y = c(0.1, 0.5), yend = c(0.7, 0.9))
```

The default behaviour of `geom_arrowsegment()` mimics that of `geom_arrowsegment(arrow = arrow())`

```
ggplot(tbl) + 
  geom_segment(aes(x = x, xend = xend, y = y, yend = yend), arrow = arrow()) + 
  xlim(c(0,1)) +
  ylim(c(0,1))
```

<img src="man/figures/geom_segment_example.png" width="400"/>

```
ggplot(tbl) + 
  geom_arrowsegment(aes(x = x, xend = xend, y = y, yend = yend)) + 
  xlim(c(0,1)) +
  ylim(c(0,1))
```

