barnes-hut
==========
A parallelized Haskell implementation of the Barnes-Hut algorithm, used to
approximate n-body simulations.

Getting Started
---------------
Requires `stack`.

```
make run
```

Background
==========
`n`-body Problem
---------------- 
Given a set of celestial bodies with mass, initial velocity,
and initial position, we would like to simulate the motion of these bodies over
time under the influence of gravity. Such simulations allow us to model the
collisions and interactions of large-scale galaxy clusters. While there is a
closed form solution for `n == 2`, no such formula exists for `n >= 3`, so
computationally expensive numerical solutions are required. These numerical
methods vary in their approaches to calculating the effect of gravity on each
body. We know from kinematics that the gravitational force on one body by
another separated by distance r is given by the following (where G is the
gravitational constant): `F = G (m1 * m2) / r^2`

Barnes-Hut Approximation
------------------------
The Barnes-Hut Approximation seeks to cut down computation by grouping very
distant masses together into one larger mass. The first step is to divide up the
`n` bodies into a quadtree (for 2D simulations) to group together nearby masses.
Then, for each body in the tree, we calculate the contribution of other bodies
in the same way as the naive algorithm. However, if a group of bodies is
sufficiently far away, we aggregate them and use their combined mass and center
of gravity for our computation. By leveraging this approximation, the
algorithm’s time complexity improves to `O(n log n)`. Whether a region is
considered "distant" or not depends on the ratio of its size to its distance
from the body. If this ratio exceeds a threshold value, the region is
approximated as above. This threshold value can be adjusted depending on desired
speed or accuracy of the simulation.

Authors
=======
Hans Montero \<hjm2133@columbia.edu\>  
Rhys Murray \<ram2269@columbia.edu\>
