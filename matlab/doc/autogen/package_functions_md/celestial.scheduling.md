# Package: celestial.scheduling


### celestial.scheduling.LAST_scheduler




### celestial.scheduling.LAST_simulator

Simulate LAST targets scheduling Package: +celestial.schedling Description:


### celestial.scheduling.assign_targets2telescopes




### celestial.scheduling.coo_visibility

Calculate the visibility of celestial coordinates Package: +celestial.scheduling Description: For a given night, and a list of targets, calculate the matrix of target visibility in 5-min (default) steps.


### celestial.scheduling.fermiexp

Fermi rise - Exp decay weight function Package: +celestial.scheduling


### celestial.scheduling.target_selection

Select a single target for one telescope Package: +celestial.scheduling Example: Res=celestial.scheduling.target_selection


### celestial.scheduling.validate_coo

Validate HA, Dec within Az,Alt and HA,Dec ranges. Package: +celestial.scheduling Description: Given a vectors of HA/Dec check for each coordinate if it complies with Az,Alt limit and withing HA/Dec ranges.


### celestial.scheduling.weight_cadence

Calculate the cadence weight for a list of targets. Package: +celestial.scheduling Description: The cadence weight is a combination of two weights, based on the last time the target was observed during the night, and


### celestial.scheduling.weights_fun

'exp' - A + B*exp(-t/C) 'fermi' - A + B/(1+ exp(-(t-C)/D))


