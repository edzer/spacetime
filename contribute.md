# contribute file

Contributions to spacetime are welcome, and will be considered for inclusion.


Contributions needed: a `generalize` method for `Track`
```
generalize(t, timeInterval = "5 minutes")
```
generalizes track `t` into 5 minute intervals;
```
generalize(t, distance = 1000)
```
generalizes the track into 1000 (distance units, usually m)
```
generalize(t, n = 20)
```
generalizes the track into segments, each having 20 points
