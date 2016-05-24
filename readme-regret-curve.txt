Cumulative Regret.

Regret is defined as: mean reward from best option - mean reward from chosen option
Cumulative regret is simply keeping track of this over time (# of choices). In engineering, I know that they try to design systems to optimally solve MAB problems, and they've figured out that the cumulative regret curve from the optimal strategy will generally be logarithmic (given a few assumptions which may not *quite* match our setup). This is from some famous paper by Lai and Robbins.

I've calculated the cumulative regret curves for both groups of both humans and ants in the dynamic environment. This is a really quick and dirty plot: it is the cumulative regret for the different groups. "Time" is simply indexed by choices. Each upward-sloping line is a different colony of ants or a different setup for the humans (I think). I didn't have time to label them, but you can see that the humans have pretty darn linear cumulative regret curves (indicating they aren't closing in on the optimal choice very well). Interestingly, the ant colonies have large variation. Some colonies are pretty linear (aka, not great at optimally exploiting), but others are very logarithmic and plateau nicely (indicating they've found the best choice and are exploiting it). The circles underneath the curves represent tandem runs. Sorry it's so messy and unappealing!

The individual ants are so bad at solving this that it's not worth plotting their curves. They're all pretty much linear and do not have very many points.
Do we have data for individual humans playing the game by themselves?