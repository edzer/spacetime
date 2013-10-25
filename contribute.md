# contribute file

Contributions to spacetime are welcome, and will be considered for inclusion.


Contributions needed: a `generalize` method for `Track` (and `Tracks`, and `TracksCollection`)
```
generalize(t, timeInterval = "5 minutes")
```
generalizes track `t` into 5 minute intervals; result is a `Track` (...) where the `sp` slot now is a `SpatialLines` object, and `time` and `endTime` reflect start and endtime of the segment.
```
generalize(t, distance = 1000)
```
generalizes the track into 1000 (distance units, usually m)
```
generalize(t, n = 20)
```
generalizes the track into segments, each having 20 points
```
generalize(t, ..., tol = 55)
```
simplifies the SpatialLines object using `rgeos::gSimplify(SL, tol = 55)`
```
generalize(t, ..., toPoints = TRUE)
```
does not keep lines, but keeps the mid point of the set over which we generalized.
