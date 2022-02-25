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

<img src="man/figures/geom_arrowsegment_example1.png" width="400"/>

The `arrows` parameter of `geom_arrowsegment()` also behaves exactly like the `arrow` parameter of `geom_segment`, as a call to `grid::arrow()`:

```
ggplot(tbl) + 
  geom_arrowsegment(aes(x = x, xend = xend, y = y, yend = yend), arrows = arrow(type = 'closed')) + 
  xlim(c(0,1)) +
  ylim(c(0,1))
```

<img src="man/figures/geom_arrowsegment_example2.png" width="400"/>

Now for the interesting bit. Suppose that we would like the arrowhead to appear at the midpoint of the segment, rather than the end. This can be done by specifying `arrow_positions = 0.5`.

```
ggplot(tbl) + 
  geom_arrowsegment(aes(x = x, xend = xend, y = y, yend = yend), arrow_positions = 0.5) + 
  xlim(c(0,1)) +
  ylim(c(0,1))
```

<img src="man/figures/geom_arrowsegment_example3.png" width="400"/>

Control of the arrow segment works as before:

```
ggplot(tbl) + 
  geom_arrowsegment(aes(x = x, xend = xend, y = y, yend = yend), arrow_positions = 0.5, arrows = arrow(type = 'closed')) + 
  xlim(c(0,1)) +
  ylim(c(0,1))
```

<img src="man/figures/geom_arrowsegment_example4.png" width="400"/>

Other aesthetics also work as you would hope:

```
tbl <- tbl %>% mutate(col = c("A", "B"))

ggplot(tbl) + 
  geom_arrowsegment(aes(x = x, xend = xend, y = y, yend = yend, col = col), arrow_positions = 0.5)  + 
  xlim(c(0,1)) +
  ylim(c(0,1))
```

<img src="man/figures/geom_arrowsegment_example5.png" width="400"/>
