


### Notes and assumptions


**Note that there are two sets of dates for the depthseries: one from the geochron datasetes, the other from the LOI datasets (which also contained radiocarbon dates)**

- original downloaded files dataset25338, dataset25345, dataset25364 were corrupted because values for attribute "MaterialDated" included commas. This was modified by hand, then placed in derivative/intermediate

- in the LOI datasets, which also have radiocarbon dates, I'm assuming that they are carbon 14

- also for the radioncarbon dates in the LOI datasets, dates are expressed as lower value/middle value/higher value. You'd think this is age- 1 SD/age/age + 1 SD...but the middle value is not exactly between the left and right values. So, I took the average of the differences between the middle value and the left/right values, and designated that as the standard deviation.