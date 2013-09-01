bbforce
=======

## Boolean Brute Force

This is a brute force solution for the first part of the following [puzzle from MIT Tech Review](http://www.technologyreview.com/sites/default/files/magazine/mitnews/puzzlecorner/MA13MITPuzzleCorner.pdf):

>Howard Cohen has plenty of AND and OR gates but just two inverters. How can he invert three signals a, b, and c?

>More generally, he wonders if the ratio I/S can ever be less than 2/3, where I is the number of inverters and S is the number of signals to invert (once again, unlimited AND and OR gates are available).

To answer the second part of the puzzle note that the obtained circuit is functionally nothing else than just three inverters. That is, having an unlimited number of ANDs and ORs we can get three inverters from two inverters. But then nothing prevents us from applying the same step again resulting in four inverters, etc. Hence, rather surprisingly the ratio can be made arbitrarily small: two inverters are enough to invert as many signals as we need!
