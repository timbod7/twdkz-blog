---
title: "An executable specification for voteflux"
author: Tim Docker
date: 2016-05-16
tags: [haskell]
---
[voteflux][1] is an interesting new political party, that will field
senate candidates at the Australian federal election in July. It's
sole policy is to implement [delegative democracy][2], and to do this
within the existing Australian political system. It intends to use
blockchain technology to provide cryptographic guarantees to the
voting process.

At the time of writing the voteflux software is incomplete, and there
is not yet a rigorous specification for how the voting system will
work. The voteflux website explains the system at a high level, but
leaves questions unanswered. Discussions in the group's slack forums
fill in some details, and the parties founders have answered some
questions of my own.

In an effort to improve my own understanding of the voteflux ideas,
and provide a basis for discussion with others, I've attempted to
write an [executable specification][3] for the system in Haskell. All
of the key logic is in [Flux.hs][4]. This was a worthwhile exercise -
having to write concrete types and corresponding code made me consider
many questions which weren't apparent when thinking less
rigourously. Going forward, I intend to build some simulations based
upon this code.

Note that this code has no endorsement from the voteflux party - it
represents my own efforts to understand the proposed system.  But I
like their plans, and hope they do well in the election.

[1]:https://voteflux.org/
[2]:https://en.wikipedia.org/wiki/Delegative_democracy
[3]:https://github.com/timbod7/flux-model
[4]:https://github.com/timbod7/flux-model/blob/master/src/Flux.hs




