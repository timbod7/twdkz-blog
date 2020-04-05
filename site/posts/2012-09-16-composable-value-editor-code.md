---
title: Composable Value Editor code
author: Tim Docker
date: 2012-09-16
tags: [programming]
---
I've made the code for this library (previously [described
here](2012-05-10-composable-value-editors.html))
available via a repository on github:
<https://github.com/timbod7/veditor> It's still experimental, so I don't
intend to put it on hackage until I have (or someone else has) a
dependency on it. The actual VE GADT in the source has an extra type
parameter intended to let the generated UIs depend on context. Where
this is not necessary, the ConstE type may be supplied. Hence, in the
actual code the type `VE ConstE a` corresponds to `VE a` in the previous
blog post.
