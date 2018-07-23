# whitechapelR

***A set of functions for tracking the hidden movement of 'Jack' in the board game Letters from Whitechapel by Fantasy Flight Games***

[![Build Status](https://travis-ci.org/bmewing/whitechapelR.svg?branch=master)](https://travis-ci.org/bmewing/whitechapelR) [![Coverage Status](https://img.shields.io/codecov/c/github/bmewing/whitechapelR/master.svg)](https://codecov.io/github/bmewing/whitechapelR?branch=master) [![CRAN\_Status\_Badge](http://www.r-pkg.org/badges/version/whitechapelR)](https://CRAN.R-project.org/package=whitechapelR) ![](http://cranlogs.r-pkg.org/badges/whitechapelR)

## Why do I want this?

There are many reasons you may want to install this package.

 * You've played the game [Letters from Whitechapel](https://boardgamegeek.com/boardgame/59959/letters-whitechapel) *not* as Jack and you've realized how incredibly broken the game is for the policemen if Jack is even halfway competent. 
 * You've played the game Letters from Whitechapel as Jack and want to learn how to make your play even more nefarious.
 * You desire advanced policing tools to help narrow down your search and level the playing field against insidious serial killers. 
 * Your thirst for justice leads you to install anything that promises to help get it.
 * You saw my [amazing Shiny app](https://markewing.shinyapps.io/whitechapel/) and wanted the package for yourself.
 * You're a little confused and think this will help you make delicious sliders (sorry, that's White Castle).

## Install it!

```r   
#Install the latest version from GitHub
#devtools::install_github("bmewing/whitechapelR")
```

## Usage

This package contains a number of functions which, when used together, allow you to map out all the possible places where Jack might be and narrow your field of search.

### Initialize a round

Each round you must indicated where the first murder occurred. This is done with the `start_round()` function which takes as it's only argument the space(s) of the murder. The result needs to be peserved.

```r
paths = start_round(64)
```

### Track Jack's movement

As Jack moves, you'll need to monitor where he might possibly be. There are two functions which help with this, `take_a_step` and `take_a_carriage` (which is a wrapper for `take_a_step`). Taking a step requires that we provide the list of all possible paths taken up to this point and which graph Jack is currently using. By default, he uses the `roads` graph but when using a lantern he uses the `alley` graph. These are both included in the package by default.

```r
#if taking a normal step
paths = take_a_step(paths,roads)

#if taking a latern special movement
# paths = take_a_step(paths,alley)
```

Additionally, you may specify via a list of blocked nodes where Jack cannot move due to being blocked by a policeman. Note, policemen cannot block alley movement but this is not hardcoded into the function, so you could possibly mess up your information at this point.

```r
#if policemen are blocking the road between 50 and 31 as well as between 30 and 13
# paths = take_a_step(paths,roads,list(c(50,31),c(30,13)))
```

Taking a carriage can only be over roads and is never blocked by policemen so it is a simple wrapper for taking two steps. Note, technically the carriage cannot move back to the space from which it originated, but that's not accounted for in v0.2

```r
# paths = take_a_carriage(paths)
```

### Looking for clues

As a policeman searching for clues, you want to be able to modify possible paths based on the evidence you aquire. This is done with the `inspect_space` function. It requires the list of all possible paths, the space(s) inspected and a boolean indicating what was found. Because of the exhaustive nature of the methods here, not finding anything can still be incredibly informative. 

```r
#if no clue was found
paths = inspect_space(paths,c(50,30),FALSE)
paths = inspect_space(paths,64,TRUE)
```

It should be pointed out that the algorithms assume you never make mistakes. Finding a clue simply eliminates all paths which do not contain that number. If you input a space which Jack could never have visited and indicate you found a clue there, the path set will be reduced to 0 and you'll have no information from which to work. There is no undo function here.

### Ending a round

Once Jack has (irritatingly) successfully made it their hideout, you want to track which spaces are candidates and improve that information over time.  This is done with the `end_round` function. By storing the endpoints of all possible paths in a variable (say `hideouts`), future rounds can use the intersection of these endpoints and previous information to, hopefully, reduce the possible spaces to a manageable number.

```r
# at the end of the first round
hideouts = end_round(paths)

# at the end of subsequent rounds
# hidouts = end_round(paths,hideouts)
```

### Visualizing Jack's movement

The final utility to mention here is the `show_board` funtion which produces an igraph representation of the game board with the appropriately numbered nodes in their identical positions. Nodes are colored based on the proportion of possible paths which flow through them (key areas to focus on) and can also highlight where hideouts might be.

```r
show_board(paths,hideouts,roads,alley,node_locations)
```
