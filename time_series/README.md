# time_series

Project to output extracted information using rolling time window approach.

The main class required a txt file with the following standard:

```
1355270609 1.80215
1355270621 1.80185
1355270646 1.80195
1355270702 1.80225
1355270702 1.80215
1355270829 1.80235
1355270854 1.80205
1355270868 1.80225
1355271000 1.80245
1355271023 1.80285
1355271024 1.80275
1355271026 1.80285
1355271027 1.80265
1355271056 1.80275
1355271428 1.80265
1355271466 1.80275
1355271471 1.80295
1355271507 1.80265
1355271562 1.80275
1355271588 1.80295
```

The expected output is a output.log file generated by logback in the following format:

```
T          V       N RS      MinV    MaxV
---------------------------------------------
1355270609 1.80215 1 1.80215 1.80215 1.80215
1355270621 1.80185 2 3.60400 1.80185 1.80215
1355270646 1.80195 3 5.40595 1.80185 1.80215
1355270702 1.80225 2 3.60420 1.80195 1.80225
1355270702 1.80215 3 5.40635 1.80195 1.80225
1355270829 1.80235 1 1.80235 1.80235 1.80235
1355270854 1.80205 2 3.60440 1.80205 1.80235
1355270868 1.80225 3 5.40665 1.80205 1.80235
1355271000 1.80245 1 1.80245 1.80245 1.80245
1355271023 1.80285 2 3.60530 1.80245 1.80285
1355271024 1.80275 3 5.40805 1.80245 1.80285
1355271026 1.80285 4 7.21090 1.80245 1.80285
1355271027 1.80265 5 9.01355 1.80245 1.80285
1355271056 1.80275 6 10.81630 1.80245 1.80285
1355271428 1.80265 1 1.80265 1.80265 1.80265
1355271466 1.80275 2 3.60540 1.80265 1.80275
1355271471 1.80295 3 5.40835 1.80265 1.80295
1355271507 1.80265 3 5.40835 1.80265 1.80295
1355271562 1.80275 2 3.60540 1.80265 1.80275
1355271588 1.80295 2 3.60570 1.80275 1.80295
```